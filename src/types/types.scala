// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

package object types {
  type address_list = helpers.scala.ListWrapper[address]
  implicit object address_listRandomizer extends helpers.scala.Random.ListWrapperRandomizer[address]
  type group_node_list = helpers.scala.ListWrapperDeep[group_node]
  implicit object group_node_listRandomizer extends helpers.scala.Random.ListWrapperDeepRandomizer[group_node]
  type nat_list = helpers.scala.ListWrapper[Int]
  implicit object nat_listRandomizer extends helpers.scala.Random.ListWrapperRandomizer[Int]
  type node_list = helpers.scala.ListWrapperDeep[node]
  implicit object node_listRandomizer extends helpers.scala.Random.ListWrapperDeepRandomizer[node]
  type path = List[String]
  implicit object pathRandomizer extends helpers.scala.Random.ListRandomizer[String]
  type queue = helpers.scala.ListWrapper[erasequeueentry]
  implicit object queueRandomizer extends helpers.scala.Random.ListWrapperRandomizer[erasequeueentry]
  type address_set = helpers.scala.SetWrapper[address]
  implicit object address_setRandomizer extends helpers.scala.Random.SetWrapperRandomizer[address]
  type key_set = helpers.scala.SetWrapper[key]
  implicit object key_setRandomizer extends helpers.scala.Random.SetWrapperRandomizer[key]
  type nat_set = helpers.scala.SetWrapper[Int]
  implicit object nat_setRandomizer extends helpers.scala.Random.SetWrapperRandomizer[Int]
  type stringset = helpers.scala.SetWrapper[String]
  implicit object stringsetRandomizer extends helpers.scala.Random.SetWrapperRandomizer[String]
  type branch_array = helpers.scala.ArrayWrapper[branch]
  implicit object branch_arrayRandomizer extends helpers.scala.Random.ArrayWrapperRandomizer[branch]
  type buffer = helpers.scala.ArrayWrapper[Byte]
  implicit object bufferRandomizer extends helpers.scala.Random.ArrayWrapperRandomizer[Byte]
  type ebatbl = helpers.scala.ArrayWrapper[ebaentry]
  implicit object ebatblRandomizer extends helpers.scala.Random.ArrayWrapperRandomizer[ebaentry]
  type key_array = helpers.scala.ArrayWrapperDeep[keyindex]
  implicit object key_arrayRandomizer extends helpers.scala.Random.ArrayWrapperDeepRandomizer[keyindex]
  type lp_array = helpers.scala.ArrayWrapperDeep[lprops]
  implicit object lp_arrayRandomizer extends helpers.scala.Random.ArrayWrapperDeepRandomizer[lprops]
  type wlarray = helpers.scala.ArrayWrapperDeep[wlentry]
  implicit object wlarrayRandomizer extends helpers.scala.Random.ArrayWrapperDeepRandomizer[wlentry]
  type zbranch_array = helpers.scala.ArrayWrapperDeep[zbranch]
  implicit object zbranch_arrayRandomizer extends helpers.scala.Random.ArrayWrapperDeepRandomizer[zbranch]
  type open_files = helpers.scala.MapWrapperDeep[Int, file]
  implicit object open_filesRandomizer extends helpers.scala.Random.MapWrapperDeepRandomizer[Int, file]
  type recoveryentries = helpers.scala.MapWrapper[lebadress, recoveryentry]
  implicit object recoveryentriesRandomizer extends helpers.scala.Random.MapWrapperRandomizer[lebadress, recoveryentry]
  type volumes = helpers.scala.MapWrapperDeep[Byte, ebatbl]
  implicit object volumesRandomizer extends helpers.scala.Random.MapWrapperDeepRandomizer[Byte, ebatbl]
  type vtbl = helpers.scala.MapWrapper[Byte, Int]
  implicit object vtblRandomizer extends helpers.scala.Random.MapWrapperRandomizer[Byte, Int]
  type wbuf_store = helpers.scala.MapWrapperDeep[Int, wbuf]
  implicit object wbuf_storeRandomizer extends helpers.scala.Random.MapWrapperDeepRandomizer[Int, wbuf]
}
