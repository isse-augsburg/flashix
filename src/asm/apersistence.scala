// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class apersistence_interface {
  def apersistence_add_gnds(LNUM: Int, GNDLIST: group_node_list, ADRLIST: address_list, ERR: Ref[error])
  def apersistence_add_ind(LNUM: Int, IND: index_node, ADR: Ref[address], ERR: Ref[error])
  def apersistence_allocate_gnd(N: Ref[Int], ERR: Ref[error])
  def apersistence_allocate_ind(N: Ref[Int], ERR: Ref[error])
  def apersistence_commit(ROOTADR0: address, MAXINO0: Int, OS0: nat_set, ERR: Ref[error])
  def apersistence_deallocate_gnd(N: Int, ERR: Ref[error])
  def apersistence_flush_gnd(LNUM: Int, ERR: Ref[error])
  def apersistence_flush_ind(LNUM: Int, ERR: Ref[error])
  def apersistence_format(VOLSIZE: Int, MAXINO0: Int, ERR: Ref[error])
  def apersistence_get_gblock_refsize(LNUM: Int, N: Ref[Int])
  def apersistence_get_gblock_size(LNUM: Int, N: Ref[Int])
  def apersistence_get_gc_block(N: Ref[Int], ERR: Ref[error])
  def apersistence_get_iblock_refsize(LNUM: Int, N: Ref[Int])
  def apersistence_get_iblock_size(LNUM: Int, N: Ref[Int])
  def apersistence_read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error])
  def apersistence_read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error])
  def apersistence_read_ind(ADR: address, IND: index_node, ERR: Ref[error])
  def apersistence_recover(ROOTADR0: Ref[address], MAXINO0: Ref[Int], NL0: nat_list, OS0: nat_set, ERR: Ref[error])
  def apersistence_requires_commit(COMMIT_ : Ref[Boolean])
  def apersistence_set_gblock_refsize(LNUM: Int, N: Int)
  def apersistence_set_iblock_refsize(LNUM: Int, N: Int)
}
