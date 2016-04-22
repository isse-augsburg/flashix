// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package integration

import types._
import types.error.error
import helpers.scala.Ref
import asm._

import scala.collection.mutable

object Debug {
  implicit class Refsizes(IS: Iterable[address]) {
    def refsizes = {
      IS.groupBy(_.lnum).map {
        case (lnum, adrs) =>
          lnum -> adrs.foldLeft(0)((n,adr) => adr.size + n)
      }
    }
  }
}

trait DebugUBIFSJournal {
  this: btree_asm =>

  def index: (Map[key, address], Map[znode, address]) = {
    val ERR = new Ref[error](error.ESUCCESS)
    val RI = mutable.Map[key, address]()
    val IS = mutable.Map[znode, address]()
    index(null, RT, ADRT, ERR, RI, IS)
    (RI.toMap, IS.toMap)
  }

  private def index(RP: znode, RT: znode, ADRT: address, ERR: Ref[error], RI: mutable.Map[key, address], IS: mutable.Map[znode, address]): Unit = {
    val R = new Ref[znode](RT)

    btree_io_load(RP, ADRT, R, ERR)
    IS += R.get -> ADRT

    var N: Int = 0

    while (N < R.get.usedsize) {
      val zbr = R.get.zbranches(N)
      val ADRC = zbr.adr
      val KEYC = zbr.key

      if (R.get.leaf) {
        RI += KEYC -> ADRC
      } else {
        index(R.get, zbr.child, ADRC, ERR, RI, IS)
      }
      N = N + 1
    }
  }
}
