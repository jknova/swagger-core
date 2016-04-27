package com.wordnik.swagger.converter

import com.wordnik.swagger.model.Model;

object ModelCache {

  val ClassMem = new collection.mutable.HashMap[(Class[_]), Option[Model]] with scala.collection.mutable.SynchronizedMap[(Class[_]), Option[Model]]

  def Memoize(cl: Class[_], result: Option[Model]): Option[Model] = {
    ClassMem += (cl) -> result
    result
  }
}
