// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

import helpers.scala._

package object types {
  type address_list = helpers.scala.ListWrapper[address]
  type group_node_list = helpers.scala.ListWrapperDeep[group_node]
  type key_list = helpers.scala.ListWrapper[key]
  type nat_list = helpers.scala.ListWrapper[Int]
  type path = List[String]
  type queue = helpers.scala.ListWrapper[erasequeueentry]
  type key_set = helpers.scala.SetWrapper[key]
  type nat_set = helpers.scala.SetWrapper[Int]
  type stringset = helpers.scala.SetWrapper[String]
  type branch_array = helpers.scala.ArrayWrapper[branch]
  type buffer = helpers.scala.ArrayWrapper[Byte]
  type ebatbl = helpers.scala.ArrayWrapper[ebaentry]
  type key_array = helpers.scala.ArrayWrapper[keyindex]
  type lp_array = helpers.scala.ArrayWrapperDeep[lprops]
  type wlarray = helpers.scala.ArrayWrapperDeep[wlentry]
  type zbranch_array = helpers.scala.ArrayWrapperDeep[zbranch]
  type open_files = helpers.scala.MapWrapperDeep[Int, file]
  type recoveryentries = helpers.scala.MapWrapperDeep[lebadress, recoveryentry]
  type volumes = helpers.scala.MapWrapperDeep[Byte, ebatbl]
  type vtbl = helpers.scala.MapWrapper[Byte, Int]
  type wbuf_store = helpers.scala.MapWrapperDeep[Int, wbuf]
}
