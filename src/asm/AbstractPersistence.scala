// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import types._
import types.error.error

abstract class AbstractPersistence {
  def persistence_add_gnd(LNUM: Int, GND: group_node, ADR: Ref[address], ERR: Ref[error])
  def persistence_add_ind(LNUM: Int, IND: index_node, ADR: Ref[address], ERR: Ref[error])
  def persistence_allocate_gnd(N: Ref[Int], ERR: Ref[error])
  def persistence_allocate_ind(N: Ref[Int], ERR: Ref[error])
  def persistence_deallocate_gnd(N: Int, ERR: Ref[error])
  def persistence_flush_gnd(LNUM: Int, ERR: Ref[error])
  def persistence_flush_ind(LNUM: Int, ERR: Ref[error])
  def persistence_format(VOLSIZE: Int, ERR: Ref[error])
  def persistence_get_gblock_refsize(LNUM: Int, N: Ref[Int])
  def persistence_get_gblock_size(LNUM: Int, N: Ref[Int])
  def persistence_get_gc_block(N: Ref[Int], ERR: Ref[error])
  def persistence_get_iblock_refsize(LNUM: Int, N: Ref[Int])
  def persistence_get_iblock_size(LNUM: Int, N: Ref[Int])
  def persistence_get_maxino(MAXINO0: Ref[Int])
  def persistence_get_root(ROOTADR0: Ref[address])
  def persistence_read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error])
  def persistence_read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error])
  def persistence_read_ind(ADR: address, IND: Ref[index_node], ERR: Ref[error])
  def persistence_read_log(NL0: nat_list)
  def persistence_read_orph(OS0: key_set, ERR: Ref[error])
  def persistence_requires_commit(COMMIT_ : Ref[Boolean])
  def persistence_set_gblock_refsize(LNUM: Int, N: Int)
  def persistence_set_iblock_refsize(LNUM: Int, N: Int)
  def persistence_write_superblock(ROOTADR0: address, MAXINO0: Int, OS0: key_set, ERR: Ref[error])
}
