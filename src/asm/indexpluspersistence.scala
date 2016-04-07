// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class indexpluspersistence_interface {
  def indexpluspersistence_add_gnd(LNUM: Int, GND: group_node, ADR: Ref[address], ERR: Ref[error])
  def indexpluspersistence_add_gnds(LNUM: Int, GNDLIST: group_node_list, ADRLIST: address_list, ERR: Ref[error])
  def indexpluspersistence_allocate_gnd(N: Ref[Int], ERR: Ref[error])
  def indexpluspersistence_commit(PMAXINO0: Int, P_OS0: nat_set, ERR: Ref[error])
  def indexpluspersistence_deallocate_gnd(N: Int, ERR: Ref[error])
  def indexpluspersistence_flush_gnd(LNUM: Int, ERR: Ref[error])
  def indexpluspersistence_format(VOLSIZE: Int, PMAXINO0: Int, ERR: Ref[error])
  def indexpluspersistence_get_gblock_refsize(LNUM: Int, N: Ref[Int])
  def indexpluspersistence_get_gblock_size(LNUM: Int, N: Ref[Int])
  def indexpluspersistence_get_gc_block(N: Ref[Int], ERR: Ref[error])
  def indexpluspersistence_index_checkdata(KEY: key, N: Int, ERR: Ref[error])
  def indexpluspersistence_index_checkkey(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error])
  def indexpluspersistence_index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error])
  def indexpluspersistence_index_entries(KEY: key, NAMES: stringset, ERR: Ref[error])
  def indexpluspersistence_index_lookup(KEY: key, EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error])
  def indexpluspersistence_index_remove(KEY: key, OLDADR: Ref[address], EXISTS: Ref[Boolean])
  def indexpluspersistence_index_store(KEY: key, ADR: address, ND: node, OLDADR: Ref[address], EXISTS: Ref[Boolean], ERR: Ref[error])
  def indexpluspersistence_index_truncate(KEY: key, N: Int, AS: address_set, ERR: Ref[error])
  def indexpluspersistence_read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error])
  def indexpluspersistence_read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error])
  def indexpluspersistence_recover(PMAXINO0: Ref[Int], PNL0: nat_list, P_OS0: nat_set, ERR: Ref[error])
  def indexpluspersistence_requires_commit(COMMIT_ : Ref[Boolean])
  def indexpluspersistence_set_gblock_refsize(LNUM: Int, N: Int)
}
