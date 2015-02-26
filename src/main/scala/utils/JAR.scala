package utils

import java.io.{File, IOException}
import java.net.{URLDecoder, URL, URISyntaxException}
import java.util
import java.util.jar.JarFile

/**
 * Created by arturas on 2015-02-26.
 */
object JAR {
  /**
   * List directory contents for a resource folder. Not recursive.
   * This is basically a brute-force implementation.
   * Works for regular files and also JARs.
   *
   * @author Greg Briggs
   * @param clazz Any java class that lives in the same place as the resources you want.
   * @param path Should end with "/", but not start with one.
   * @return Just the name of each member item, not the full paths.
   * @throws URISyntaxException
   * @throws IOException
   */
  @throws(classOf[URISyntaxException])
  @throws(classOf[IOException])
  def getResourceListing(clazz: Class[_], path: String): Array[String] = {
    var dirURL: URL = clazz.getClassLoader.getResource(path)
    if (dirURL != null && (dirURL.getProtocol == "file")) {
      return new File(dirURL.toURI).list
    }
    if (dirURL == null) {
      val me: String = clazz.getName.replace(".", "/") + ".class"
      dirURL = clazz.getClassLoader.getResource(me)
    }
    if (dirURL.getProtocol == "jar") {
      val jarPath: String = dirURL.getPath.substring(5, dirURL.getPath.indexOf("!"))
      val jar: JarFile = new JarFile(URLDecoder.decode(jarPath, "UTF-8"))
      val entries = jar.entries
      val result = new util.HashSet[String]
      while (entries.hasMoreElements) {
        val name: String = entries.nextElement.getName
        if (name.startsWith(path)) {
          var entry: String = name.substring(path.length)
          val checkSubdir: Int = entry.indexOf("/")
          if (checkSubdir >= 0) {
            entry = entry.substring(0, checkSubdir)
          }
          result.add(entry)
        }
      }
      return result.toArray(new Array[String](result.size))
    }
    throw new UnsupportedOperationException("Cannot list files for URL " + dirURL)
  }
}
