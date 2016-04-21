package integration

import asm._
import types._
import types.error._
import helpers.scala._

class Flashix(mtd: mtd_interface)(implicit val ops: algebraic.Algebraic, val procs: proc.Procedures) {
  // Check axioms
  procs.flashix_check_axioms

  val ubiio = new ubi_io_asm(mtd)
  val ubi = new ubi_asm(new volumes(), new queue(), 0, new wlarray(), new nat_set(), 0, ubiio)
  val wbuf = new wbuf_asm(0, new wbuf_store(), ubi)
  val persistence_io = new persistence_io_asm(superblock.uninit, 0, wbuf)
  val persistence = new persistence_asm(binheap(new key_array(), 0), new nat_list(), new nat_list(), new lp_array(), persistence_io)
  val btree = new btree_asm(znode.uninit, address(0, 0, 0), persistence)
  val journal = new gjournal_asm(0, false, new nat_set(), 0, true, 0, btree) // TODO: sync option
  val aubifs = new aubifs_asm(journal)
  val vfs = new vfs_asm(new open_files(), 0, aubifs)
  
  def posix: posix_interface = vfs
}
