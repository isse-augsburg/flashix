// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class aubifs_core_interface {
  def check_commit(ERR: Ref[error])
  def commit(ERR: Ref[error])
  def format(VOLSIZE: Int, DOSYNC: Boolean, ERR: Ref[error])
  def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error])
  def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error])
  def index_lookup(KEY: key, EXISTS: Ref[Boolean], ND: Ref[node], ERR: Ref[error])
  def index_newino(KEY: Ref[key])
  def index_remove(KEY: key)
  def index_store(KEY: key, ADR: address)
  def index_truncate(KEY: key, N: Int)
  def journal_add1(ND1: node, ADR1: Ref[address], ERR: Ref[error])
  def journal_add2(ND1: node, ND2: node, ADR1: Ref[address], ADR2: Ref[address], ERR: Ref[error])
  def journal_add3(ND1: node, ND2: node, ND3: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ERR: Ref[error])
  def journal_add4(ND1: node, ND2: node, ND3: node, ND4: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ADR4: Ref[address], ERR: Ref[error])
  def journal_add5(ND1: node, ND2: node, ND3: node, ND4: node, ND5: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ADR4: Ref[address], ADR5: Ref[address], ERR: Ref[error])
  def journal_gc()
  def journal_get(ADR: address, ND: Ref[node], ERR: Ref[error])
  def journal_sync(ERR: Ref[error])
  def orphans_contains(KEY: key, EXISTS: Ref[Boolean])
  def orphans_insert(KEY: key)
  def orphans_remove(KEY: key)
  def recover(DOSYNC: Boolean, AX: address_list, KS: key_set, ERR: Ref[error])
}
