package de.uni_saarland.cs.se
package runtime

import utils.ConfigurationError
import utils.{ListStorage, MapStorage, Storage, StorageType}
import java.util.Scanner
import scala.collection.mutable

/**
 * Configuration class for databases.
 * 
 * @param read whether the database should support read operations
 * @param write whether the database should support write operations
 * @param transaction whether the database should support transactions
 * @param logging whether the database should support logging
 * @param storageType the type of storage the database should use
 */
class DatabaseConfig(
  val read: Boolean,
  val write: Boolean,
  val transaction: Boolean,
  val logging: Boolean,
  val storageType: StorageType
) {}

 
/**
 * A runtime-configurable version of our database SPL.
 * 
 * @param config the configuration for the database
 */
class Database(val config: DatabaseConfig){
  var storageType = config.storageType
  var Storage = if (config.storageType.toString == "MAP") utils.MapStorage() else utils.ListStorage()
  var tempstorage = utils.ListStorage() 
  def read(key:String):  Option[String] =
    if (config.logging == true) println(s"Reading value for key '$key'.") 
    Storage.get(key)
  
  def write(key:String,content:String): Unit = 
    if (config.write== false) throw new ConfigurationError()
    if (config.write== true){
      if (config.logging == true)  println(s"Writing value '$content' at key '$key'.")
      if (config.transaction == true) tempstorage.put(key, content) else Storage.put(key, content)
      }
      
    
  def commit(): Any =  
    if (config.transaction== false) throw new ConfigurationError()
    if (config.transaction== true){
      var size = tempstorage.size()
      if (config.logging == true)  println(s"Committing $size entries.")
      tempstorage.foreach((key, content) => Storage.put(key, content))
      tempstorage = ListStorage()
      size = tempstorage.size()
      }
   
  def rollback(): Any=
    if (config.transaction== false) throw new ConfigurationError()
    if (config.transaction== true){
      var size = tempstorage.size()
      if ((config.logging) == true) println(s"Rolling back ${size} entries.")
      tempstorage = ListStorage()
      size = tempstorage.size()
      }
  

}
   
