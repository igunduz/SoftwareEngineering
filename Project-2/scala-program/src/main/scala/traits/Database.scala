package de.uni_saarland.cs.se
package traits

import utils.{ListStorage, MapStorage, Storage, StorageType}

import scala.collection.mutable


/**
 * Database interface for all database implementations and traits.
 */
trait Database {
  /**
   * The database's storage type, i.e., MAP or LIST.
   */
  val storageType: StorageType

  /**
   * Gives subclasses and traits access to the database's storage.
   *
   * @return the database's storage
   */
  def storage(): Storage
  var tempstorage : Storage = ListStorage()
}

trait MapStoreDatabase extends Database  {
   override val storageType =  StorageType.MAP
   override def storage(): Storage = MapStorage()
   //var tempstorage = ListStorage()
}

trait ListStoreDatabase extends Database {
   override val storageType =  StorageType.LIST
   override def storage(): Storage = ListStorage()
   //var tempstorage = ListStorage()
}

trait Read extends Database {
   var store = storage()
   def read(key: String) : Option[String] = store.get(key)
}

trait Write extends Database with Read{
  def write(key: String, content: String): Unit  = 
     //tempstorage.put(key, content)
     store.put(key, content)
}

trait Transaction extends Database with Write with Read{
   override def write(key: String, content: String): Unit  = 
    tempstorage.put(key, content)
   def commit() : Any = 
     var size = tempstorage.size()
     tempstorage.foreach((key, content) => store.put(key, content))
     size = tempstorage.size()
   def rollback(): Any = 
     var size = tempstorage.size()
     tempstorage = ListStorage()
     size = tempstorage.size()
}

trait ReadWithLogging extends Database{
   var store = storage()
   def read(key: String) : Option[String] = 
    println(s"Reading value for key '$key'.")
    store.get(key)
}

trait WriteWithLogging extends Database with ReadWithLogging{
  def write(key: String, content: String): Unit  = 
    println(s"Writing value '$content' at key '$key'.")
    store.put(key, content)
}

trait TransactionWithLogging extends Database with ReadWithLogging with WriteWithLogging{
  override def write(key: String, content: String): Unit  = 
    println(s"Writing value '$content' at key '$key'.")
    tempstorage.put(key, content) 
  def commit() : Any = 
    var size = tempstorage.size()
    println(s"Committing $size entries.")
    tempstorage.foreach((key, content) => store.put(key, content))
  def rollback(): Any =  
    var size = tempstorage.size()
    println(s"Rolling back ${size} entries.")
    tempstorage = ListStorage()
    size = tempstorage.size()
}





