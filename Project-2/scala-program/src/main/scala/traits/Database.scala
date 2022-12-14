package de.uni_saarland.cs.se
package traits

import utils.{ListStorage, MapStorage, Storage, StorageType}
import utils.ConfigurationError
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
  protected def storage(): Storage
  def read(key: String): Any
  def write(key: String, value: String): Any
  def commit(): Any
  def rollback(): Any
}

class MapStoreDatabase extends Database{
   //private val Storage: Storage = MapStorage()
   override val storageType =  StorageType.MAP
   override protected def storage(): Storage = MapStorage()
   override def read(key: String) : Option[String] = storage().get(key)
   override def write(key: String, value: String): Any = ConfigurationError()
   override def commit(): Any = ConfigurationError()
   override def rollback(): Any = ConfigurationError()
}
 
class ListStoreDatabase extends Database{
   //private val Storage: Storage = ListStorage()
   override val storageType =  StorageType.LIST
   override protected def storage(): Storage = ListStorage()
   override def read(key: String) :  Option[String] = storage().get(key)
   override def write(key: String, value: String): Any = ConfigurationError()
   override def commit(): Any = ConfigurationError()
   override def rollback(): Any = ConfigurationError()
}


trait Read extends Database{
  override def read(key: String) : Option[String] = storage().get(key)
}

trait Write extends Database{
  override def write(key: String, content: String): Unit  = 
    storage().put(key, content)
}

trait Transaction extends Database{
  val tempstorage = ListStorage()
  override def commit() : Any = tempstorage.size()
  override def rollback(): Any = tempstorage.size()
}

trait TransactionWithLogging extends Database{
  val tempstorage : Storage = ListStorage()
  override def commit() : Any = 
    val size : Int = tempstorage.size()
    println(s"Committing $size entries.")
    tempstorage.foreach((k, v) => storage().put(k, v))
    //tempstorage = ListStorage()
  override def rollback(): Any = 
    val size = tempstorage.size()
    println(s"Rolling back ${size} entries.")
}

trait WriteWithLogging extends Database{
  override def write(key: String, content: String): Any  = 
    storage().put(key, content)
    println(s"Writing value '$content' at key '$key'.")
    
}

trait ReadWithLogging extends Database{
  override def read(key: String) : Option[String] = 
    println(s"Reading value for key '$key'.")
    storage().get(key)
}