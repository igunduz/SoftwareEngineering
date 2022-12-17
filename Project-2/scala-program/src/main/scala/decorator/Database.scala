package de.uni_saarland.cs.se
package decorator

import utils.ConfigurationError
import utils.{ListStorage, MapStorage, Storage, StorageType}

import scala.collection.mutable


/**
 * Interface for the database components and decorators.
 */
trait Database {
  def read(key: String): Any
  def write(key: String, value: String): Any
  def commit(): Any
  def rollback(): Any
  val storageType: StorageType
  private[decorator] def Storage(): Storage
}
 
 class  MapStorageDatabase extends Database{
   override def read(key: String) : Any = throw new ConfigurationError()
   override def write(key: String, content: String): Any  = throw new ConfigurationError()
   override def commit() : Any = throw new ConfigurationError()
   override def rollback(): Any = throw new ConfigurationError()
   override val storageType =  StorageType.MAP
   override private[decorator] def Storage(): Storage = MapStorage()
}
 
 
 class  ListStorageDatabase extends Database{
   override def read(key: String) : Any = throw new ConfigurationError()
   override def write(key: String, content: String): Any  = throw new ConfigurationError()
   override def commit() : Any = throw new ConfigurationError()
   override def rollback(): Any = throw new ConfigurationError()
   val storageType =  StorageType.LIST
   override private[decorator]  def Storage(): Storage = ListStorage()
}
 
 class Read(val database:Database) extends Database{
    private var storage = Storage()
    def read(key: String) : Option[String] = storage.get(key)
    def write(key: String, content: String) : Any  = throw new ConfigurationError()
    def commit() : Any = throw new ConfigurationError()
    def rollback(): Any  = throw new ConfigurationError()
    val storageType = if (database.storageType.toString == "MAP") StorageType.MAP else StorageType.LIST
    override private[decorator] def Storage(): Storage = if (database.storageType.toString == "MAP") MapStorage() else ListStorage()
}
 
 
 class Write(val database:Database) extends Database{
   private var storage = Storage()
   override def read(key: String) : Option[String] = storage.get(key)
   override def write(key: String, content: String) : Unit = storage.put(key, content)
   override def commit() : Any = throw new ConfigurationError()
   override def rollback(): Any = throw new ConfigurationError()
   val storageType = if (database.storageType.toString == "MAP") StorageType.MAP else StorageType.LIST
   override private[decorator] def Storage(): Storage = if (database.storageType.toString == "MAP") MapStorage() else ListStorage()
}
 
 class Transaction(val database:Database) extends Database{
   private var tempstorage = ListStorage() 
   private var storage = Storage()
   override def read(key: String) : Option[String] = storage.get(key)
   override def write(key: String, content: String) : Unit = tempstorage.put(key, content)
   override def commit() : Any = 
     tempstorage.size()
     tempstorage.foreach((k, v) => storage.put(k, v))
     tempstorage = ListStorage()
   override def rollback(): Any = 
     var size = tempstorage.size()
     tempstorage = ListStorage()
     size = tempstorage.size()
   override val storageType = if (database.storageType.toString == "MAP") StorageType.MAP else StorageType.LIST
   override private[decorator] def Storage(): Storage = if (database.storageType.toString == "MAP") MapStorage() else ListStorage()
}
 
 class Logging(val database:Database) extends Database{
   private var tempstorage = ListStorage() 
   private var storage = Storage()
   override def read(key: String) : Unit = 
     storage.get(key)
     println(s"Reading value for key '$key'.")
   override def write(key: String, content: String) : Any = 
     storage.put(key, content)
     tempstorage.put(key, content)
     println(s"Writing value '$content' at key '$key'.")
   override def commit() : Any = 
     val size = tempstorage.size()
     println(s"Committing $size entries.")
   override def rollback(): Any = 
     var size = tempstorage.size()
     println(s"Rolling back ${size} entries.")
     tempstorage = ListStorage()
     size = tempstorage.size()
   override val storageType =  if (database.storageType.toString == "MAP") StorageType.MAP else StorageType.LIST
   override private[decorator] def Storage(): Storage = if (database.storageType.toString == "MAP") MapStorage() else ListStorage()
}
 
 