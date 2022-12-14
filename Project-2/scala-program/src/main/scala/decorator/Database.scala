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
  def Storage(): Storage
  var tempstorage : Storage
}
 
 class  MapStorageDatabase extends Database{
   @throws(classOf[Exception])
   //private val Storage: Storage = MapStorage()
   var tempstorage = ListStorage() 
   override def read(key: String) : Option[String] = Storage().get(key)
   override def write(key: String, content: String): Any  = 
     try{
       Storage().put(key, content)
     } catch{
        case e: Exception => ConfigurationError()
     }
   override def commit() : Any = 
     try{
       tempstorage.size() 
     } catch{
        case e: Exception => ConfigurationError()
     }
   override def rollback(): Any = 
     try{
       tempstorage.size()
     } catch{
        case e: Exception => ConfigurationError()
     }
   val storageType =  StorageType.MAP
   override def Storage(): Storage = MapStorage()
}
 
 
 class  ListStorageDatabase extends Database{
   @throws(classOf[Exception])
   //private val Storage: Storage = utils.ListStorage()
   var tempstorage = ListStorage() 
   override def read(key: String) : Option[String] = Storage().get(key)
   override def write(key: String, content: String): Any  = 
     try{
       Storage().put(key, content)
     } catch{
        case e: Exception => ConfigurationError()
     }
   override def commit() : Any = 
     try{
       tempstorage.size() 
     } catch{
        case e: Exception => ConfigurationError()
     }
   override def rollback(): Any = 
     try{
       tempstorage.size()
     } catch{
        case e: Exception => ConfigurationError()
     }
   val storageType =  StorageType.LIST
   override def Storage(): Storage = utils.ListStorage()
}
 
 class Read(val database:Database) extends Database{
   //protected val Storage: Storage = if (database.toString == "MapStorageDatabase") MapStorage() else ListStorage() 
   var tempstorage = ListStorage() 
   override def read(key: String) : Option[String] = Storage().get(key)
   override def write(key: String, content: String) : Any  = None
   override def commit() : Any = None
   override def rollback(): Any  = None
   override val storageType = if (database.toString == "MapStorageDatabase") StorageType.MAP else StorageType.LIST
   override  def Storage(): Storage = if (database.toString == "MapStorageDatabase") MapStorage() else ListStorage()
}
 
 
 class Write(val database:Database) extends Database{
   //protected val Storage: Storage = if (database.toString == "MapStorageDatabase") utils.MapStorage() else utils.ListStorage()
   var tempstorage = ListStorage() 
   override def read(key: String) : Option[String] = Storage().get(key)
   override def write(key: String, content: String) : Unit = Storage().put(key, content)
   override def commit() : Any = None
   override def rollback(): Any = None
   override val storageType = if (database.toString == "MapStorageDatabase") StorageType.MAP else StorageType.LIST
   override def Storage(): Storage = if (database.toString == "MapStorageDatabase") utils.MapStorage() else utils.ListStorage()
}
 
 class Transaction(val database:Database) extends Database{
   //protected val Storage: Storage = if (database.toString == "MapStorageDatabase") utils.MapStorage() else utils.ListStorage() 
   var tempstorage = database.tempstorage
   override def read(key: String) : Option[String] = Storage().get(key)
   override def write(key: String, content: String) : Unit = tempstorage.put(key, content)
   override def commit() : Any = 
     tempstorage.size()
     tempstorage.foreach((k, v) => Storage().put(k, v))
     tempstorage = ListStorage()
   override def rollback(): Any = tempstorage.size()
   override val storageType = if (database.toString == "MapStorageDatabase") StorageType.MAP else StorageType.LIST
   override def Storage(): Storage = if (database.toString == "MapStorageDatabase") utils.MapStorage() else utils.ListStorage()
}
 
 class Logging(val database:Database) extends Database{
   //protected val Storage: Storage = if (database.toString == "MapStorageDatabase") utils.MapStorage() else utils.ListStorage() 
   var tempstorage = ListStorage() 
   override def read(key: String) : Unit = 
     Storage().get(key)
     println(s"Reading value for key '$key'.")
   override def write(key: String, content: String) : Any = 
     Storage().put(key, content)
     println(s"Writing value '$content' at key '$key'.")
   override def commit() : Any = 
     val size = tempstorage.size()
     println(s"Committing $size entries.")
   override def rollback(): Any = 
     val size = tempstorage.size()
     println(s"Rolling back ${size} entries.")
   override val storageType = if (database.toString == "MapStorageDatabase") StorageType.MAP else StorageType.LIST
   override def Storage(): Storage = if (database.toString == "MapStorageDatabase") utils.MapStorage() else utils.ListStorage()
}
 
 