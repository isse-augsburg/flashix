package asm

import helpers.scala._
import types._
import types.error.error

abstract class AUBIFSJournal {
  def aubifs_commit(ERR: Ref[error])
  def aubifs_internal_check_commit(ERR: Ref[error])
  def aubifs_internal_format(VOLSIZE: Int, ERR: Ref[error])
  def aubifs_readflash(AX: address_list, KS: key_set, ERR: Ref[error])
  def index_checkdata(KEY: key, N: Int, ERR: Ref[error])
  def index_checkkey(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error])
  def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error])
  def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error])
  def index_lookup(KEY: key, EXISTS: Ref[Boolean], ND: Ref[node], ERR: Ref[error])
  def index_newino(KEY: Ref[key], ERR: Ref[error])
  def index_remove(KEY: key)
  def index_store(KEY: key, ADR: address, ND: node, ERR: Ref[error])
  def index_truncate(KEY: key, N: Int, ERR: Ref[error])
  def journal_add1(ND1: node, ADR1: Ref[address], ERR: Ref[error])
  def journal_add2(ND1: node, ND2: node, ADR1: Ref[address], ADR2: Ref[address], ERR: Ref[error])
  def journal_add3(ND1: node, ND2: node, ND3: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ERR: Ref[error])
  def journal_add4(ND1: node, ND2: node, ND3: node, ND4: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ADR4: Ref[address], ERR: Ref[error])
  def journal_gc(ERR: Ref[error])
  def journal_get(ADR: address, ND: Ref[node], ERR: Ref[error])
  def orphan_insert(KEY: key)
  def orphan_remove(KEY: key)
  def orphans_contains(KEY: key, EXISTS: Ref[Boolean])
}
