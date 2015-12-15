package types

import helpers.scala._

sealed abstract class node extends DeepCopyable[node] {
  def key : key = throw new InvalidSelector("key undefined")
  def updated_key(__x : key) : node = throw new InvalidSelectorUpdate("updated_key undefined")
  def meta : metadata = throw new InvalidSelector("meta undefined")
  def updated_meta(__x : metadata) : node = throw new InvalidSelectorUpdate("updated_meta undefined")
  def directory : Boolean = throw new InvalidSelector("directory undefined")
  def updated_directory(__x : Boolean) : node = throw new InvalidSelectorUpdate("updated_directory undefined")
  def nlink : Int = throw new InvalidSelector("nlink undefined")
  def updated_nlink(__x : Int) : node = throw new InvalidSelectorUpdate("updated_nlink undefined")
  def size : Int = throw new InvalidSelector("size undefined")
  def updated_size(__x : Int) : node = throw new InvalidSelectorUpdate("updated_size undefined")
  def ino : Int = throw new InvalidSelector("ino undefined")
  def updated_ino(__x : Int) : node = throw new InvalidSelectorUpdate("updated_ino undefined")
  def data : buffer = throw new InvalidSelector("data undefined")
  def updated_data(__x : buffer) : node = throw new InvalidSelectorUpdate("updated_data undefined")
}

object node {
  implicit object Randomizer extends helpers.scala.Randomizer[node] {
    def random() : node = inodenode(helpers.scala.Random[key], helpers.scala.Random[metadata], helpers.scala.Random[Boolean], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }

  /**
   * case-classes and objects for constructors
   */
  final case class inodenode(override val key : key, override val meta : metadata, override val directory : Boolean, override val nlink : Int, override val size : Int) extends node {
    override def updated_key(__x : key) : inodenode = copy(key = __x)
    override def updated_meta(__x : metadata) : inodenode = copy(meta = __x)
    override def updated_directory(__x : Boolean) : inodenode = copy(directory = __x)
    override def updated_nlink(__x : Int) : inodenode = copy(nlink = __x)
    override def updated_size(__x : Int) : inodenode = copy(size = __x)
    override def deepCopy(): node = inodenode(key, meta, directory, nlink, size)
  }
  final case class dentrynode(override val key : key, override val ino : Int) extends node {
    override def updated_key(__x : key) : dentrynode = copy(key = __x)
    override def updated_ino(__x : Int) : dentrynode = copy(ino = __x)
    override def deepCopy(): node = dentrynode(key, ino)
  }
  final case class datanode(override val key : key, override val data : buffer) extends node {
    override def updated_key(__x : key) : datanode = copy(key = __x)
    override def updated_data(__x : buffer) : datanode = copy(data = __x)
    override def deepCopy(): node = datanode(key, data.deepCopy)
  }
  final case class truncnode(override val key : key, override val size : Int) extends node {
    override def updated_key(__x : key) : truncnode = copy(key = __x)
    override def updated_size(__x : Int) : truncnode = copy(size = __x)
    override def deepCopy(): node = truncnode(key, size)
  }

  def uninit = inodenode(key.uninit, metadata.uninit, helpers.scala.Boolean.uninit, 0, 0)
}
