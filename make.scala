
import java.io.{FileOutputStream, FileWriter, File}
import java.net.URL
import java.nio.ByteBuffer
import java.nio.channels.Channels
import scala.collection.JavaConversions._
import java.util.jar.{JarOutputStream, JarFile}
import scala.sys.process.Process




private object LibraryProvider {

  def main(args:Array[String]) {
    try {
      args.toList match {
        case scalaVersion :: additionalLibraries =>
        downloadAndPrepareScala(scalaVersion, additionalLibraries)
      case _ => printUsageHelp()
    }
  } catch {
    case ex: Exception =>
      println(ex)
      printUsageHelp()
  }
  }

  /**
   * Prints out usage help.
   */
  private def printUsageHelp() {
    println((
      """
        |USAGE: scala make.scala <scala-version> [additional-library ...]
        |
        |Downloads and the scala libraries from maven central in the specified version,
        |splits the base library into smaller parts and compiles them all to DalvikVM bytecode
        |
        |<scala-version>  the scala version to make. Must be available in maven central
        |
        |[library-parts]  further version dependent libraries from org.scala-lang.
        |                 example: scala-actors or scala-reflect """).stripMargin)
  }

  def downloadAndPrepareScala(version:String, additionalLibraries:Seq[String]) {
    val baseFolder = new File(s"scala/$version/lib")
    baseFolder.mkdirs()
    val permissionFolder = new File(s"scala/$version/permissions")
    permissionFolder.mkdirs()

    val baseLibJar = loadMavenArtifact("org.scala-lang","scala-library",version)

    val partNames = Seq("collection.mutable","collection.immutable","collection")
    val baseLibJars = splitJarFile(baseLibJar,"scala",partNames)
    val dexedBaseLibJars = baseLibJars.map(f=> (f._1,dexJarFile(f._2,new File(baseFolder, s"scala-library.${f._1}.jar"))))
    dexedBaseLibJars.map(f=> createPermissionXml(f._2,permissionFolder,f._1,version))

    for (additionalLibrary <- additionalLibraries) {
      val mavenArtifact = loadMavenArtifact("org.scala-lang",additionalLibrary,version)
      val dexedMavenArtifact = dexJarFile(mavenArtifact, new File(baseFolder,s"$additionalLibrary.jar"))
      createPermissionXml(dexedMavenArtifact,permissionFolder,additionalLibrary,version)
    }

  }


  private def createPermissionXml(library:File, targetFolder:File, name:String, version:String) = {
    val fileNameOnTarget = s"/system/framework/scala/$version/${library.getName}"
    val libName = s"$name-$version"
    val targetFile = new File(targetFolder,s"$libName.xml")
    val targetFileWriter = new FileWriter(targetFile)
    targetFileWriter.append(xmlPermissionDescriptor(libName,fileNameOnTarget))
    targetFileWriter.close()
    targetFile
  }

  private def xmlPermissionDescriptor(libName:String, fileName:String) = {
    s"""<permissions>
      <library name="$libName" file="$fileName" />
    </permissions>"""
  }

  private def dexJarFile(src:File, dest:File) = {
    Process.apply(s"""dx.bat -JXmx1024M -JXms1024M -JXss4M --no-optimize --debug --dex --output=${dest.getAbsolutePath} ${src.getAbsolutePath}""").!
    dest
  }

  private def extractPackageName(fileName:String) = """([a-z/]+)/.*""".r.findFirstMatchIn(fileName).map(_.group(1)).getOrElse("base").replace("/",".")

  private def distribute(default:String,values:Iterable[String],candidate:String) = values.find(v=>candidate.startsWith(v)).getOrElse(default)

  private def splitJarFile(bigJarFile:File, basePackage:String, splitPackages:Seq[String]) = {

    val fullSplitPackages = splitPackages.map(sub=>s"$basePackage.$sub")

    val jar = new JarFile(bigJarFile)

    val packageMap = jar.entries.toIterable.filter(classFile=> !classFile.isDirectory && classFile.getName != "META-INF/MANIFEST.MF")
      .groupBy(f=>distribute("scala", fullSplitPackages, extractPackageName(f.getName)))

    for (p<-packageMap) yield {
      val (partName,partFiles) = p

      val partialJarFile = File.createTempFile(s"${bigJarFile.getName.replace(".jar","")}.$partName",".jar")
      partialJarFile.deleteOnExit()

      val partialJar = new JarOutputStream( new FileOutputStream(partialJarFile),jar.getManifest)

      for (jarFileEntry<-partFiles) {
        val clazzInputChannel = Channels.newChannel(jar.getInputStream(jarFileEntry))
        val buff = ByteBuffer.allocate(32*1024)
        partialJar.putNextEntry(jarFileEntry)

        while (clazzInputChannel.read(buff) > 0) {
          partialJar.write(buff.array,0,buff.position)
          buff.flip
        }
      }
      partialJar.close()
      (partName,partialJarFile)
    }
  }


  private def loadMavenArtifact(groupId:String, artifactName:String, version:String) = {
    val targetFile = File.createTempFile(s"mvn-$groupId.$artifactName-$version",".jar")
    targetFile.deleteOnExit()
    val groupPath = groupId.replace('.','/')
    val url = new URL(s"http://repo1.maven.org/maven2/$groupPath/$artifactName/$version/$artifactName-$version.jar")
    val channel = Channels.newChannel(url.openStream())
    val fos = new FileOutputStream(targetFile)
    fos.getChannel.transferFrom(channel,0,Long.MaxValue)
    fos.close()
    targetFile
  }

}

LibraryProvider.main(args)