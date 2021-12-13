/**
 * cse250.pa2.SortedList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:haiyizha
 * Person#:50287269
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.adaptors.LectureStack
import cse250.list.{ImmutableLinkedList, ListNode}
import cse250.types.immutable.ListADT

import scala.util.Sorting

class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------

  var inlist: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  var checkin = 0
  var relist: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  var checkre = 0
  var prlist: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  var checkpr = 0

  /** Gets element at position i within the list. */
  override def apply(i: Int): A = {
    require(i<_storageList.length && i >=0,"IllegalArgumentException")
    _storageList(i)
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = {
    _storageList.length
  }

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = {
    _storageList.iterator
  }

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit ={    //10
    inlist = _storageList
    var dex = 0
    if(_storageList.length == 0){
      _storageList = _storageList.inserted(0,elem)
    }
    else{
      for(i <- 0 until _storageList.length){
        if(_comp.gteq(elem,_storageList.apply(i))){   //elem >= i
          dex = i+1}

      }
      _storageList = _storageList.inserted(dex,elem)
    }
    checkin = 10
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return true if any change has been made, and false otherwise.
   */
  def remove(elem: A): Boolean = {  //11
    relist = _storageList
    var num = 0
    var check = 0
    for (i <- 0 until _storageList.length) {
      if (_comp.equiv(elem,_storageList.apply(i))) {
        num = i
        check += 1}}
    for(i <- 0 until check){
      if (_storageList.length == 0) {
        return false}
      else {
        for (i <- 0 until _storageList.length) {
          if (_comp.equiv(elem,_storageList.apply(i))) {
            num = i}}
        _storageList = _storageList.removed(num)}}

    if(check != 0){
      checkin = 11
      return true}
    else{
      return false}}

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = { //12
    prlist = _storageList
    while (!operations.isEmpty){
      if (operations.front._1 == "insert") {
        this.insert(operations.front._2)
        operations.dequeue
      }
      else if (operations.front._1 == "remove") {
        this.remove(operations.front._2)
        operations.dequeue
      }
      checkin = 12
    }
  }
  /** Undo the last modification, if any change has been made.
   * If no change to undo exists, throw an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    require(!_storageList.isEmpty,"IllegalArgumentException")
    if(checkin == 10){
      _storageList = inlist
    }
    else if (checkin == 11){
      _storageList = relist
    }
    else if(checkin == 12){
      _storageList = prlist
    }
  }
}
