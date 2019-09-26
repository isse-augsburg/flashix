// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

abstract class ApersistenceInterface extends ASM {
  def add_gnds(GNDLIST: group_node_list, ADRLIST: address_list, ERR: Ref[error])
  def add_ind(IND: index_node, ADR: Ref[address], ERR: Ref[error])
  def allocate_gnd(ERR: Ref[error])
  def allocate_ind(ERR: Ref[error])
  def commit(ROOTADR0: address, MAXINO0: Int, OS0: nat_set, ERR: Ref[error])
  def deallocate_gnd(N: Int, ERR: Ref[error])
  def format(VOLSIZE: Int, SIZE: Int, MAXINO0: Int, ERR: Ref[error])
  def get_block_free_size(N: Ref[Int])
  def get_gblock_refsize(LNUM: Int, N: Ref[Int])
  def get_gc_block(N: Ref[Int], ERR: Ref[error])
  def get_iblock_refsize(LNUM: Int, N: Ref[Int])
  def get_leb_size(N: Ref[Int])
  def is_log_empty(EMPTY_ : Ref[Boolean])
  def is_readonly(ROFS: Ref[Boolean])
  def read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error])
  def read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error])
  def read_ind(ADR: address, IND: Ref[index_node], ERR: Ref[error])
  def recover(ROOTADR0: Ref[address], MAXINO0: Ref[Int], NL0: nat_list, OS0: nat_set, ERR: Ref[error])
  def requires_commit(COMMIT_ : Ref[Boolean])
  def set_gblock_refsize(LNUM: Int, N: Int)
  def set_iblock_refsize(LNUM: Int, N: Int)
  def sync(ERR: Ref[error])
}
