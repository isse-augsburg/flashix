// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

abstract class IndexInterface extends ASM {
  def add_gnds(GNDLIST: group_node_list, ADRLIST: address_list, ERR: Ref[error])
  def allocate_gnd(ERR: Ref[error])
  def commit(PMAXINO0: Int, P_OS0: nat_set, ERR: Ref[error])
  def deallocate_gnd(N: Int, ERR: Ref[error])
  def format(VOLSIZE: Int, SIZE: Int, PMAXINO0: Int, ERR: Ref[error])
  def get_block_free_size(N: Ref[Int])
  def get_gblock_refsize(LNUM: Int, N: Ref[Int])
  def get_gc_block(N: Ref[Int], ERR: Ref[error])
  def get_leb_size(N: Ref[Int])
  def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error])
  def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error])
  def index_lookup(KEY: key, EXISTS: Ref[Boolean], ADR: Ref[address], ERR: Ref[error])
  def index_remove(KEY: key, OLDADR: Ref[address], EXISTS: Ref[Boolean])
  def index_store(KEY: key, ADR: address, OLDADR: Ref[address], EXISTS: Ref[Boolean])
  def index_truncate(KEY: key, N: Int, AS: address_set)
  def is_log_empty(EMPTY_ : Ref[Boolean])
  def read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error])
  def read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error])
  def recover(PMAXINO0: Ref[Int], PNL0: nat_list, P_OS0: nat_set, ERR: Ref[error])
  def requires_commit(COMMIT_ : Ref[Boolean])
  def set_gblock_refsize(LNUM: Int, N: Int)
  def sync(ERR: Ref[error])
}
