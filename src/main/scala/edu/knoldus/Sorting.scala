package edu.knoldus

import scala.annotation.tailrec
import scala.util.control.Breaks.{break, breakable}

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    for ( j <- 1 until array.length ) {
      breakable {
        for ( i <- (1 to j).reverse ) {
          if (array(i-1) < array(i)) {
            break
          } else {
            val temp = array(i)
            array(i) = array(i-1)
            array(i-1) = temp
          }
        }
      }
    }
    array
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def selectSortHelper(list:List[Int], accumList:List[Int] = List[Int]()): List[Int] = {

      list match {
        case Nil => accumList
        case _ =>
          val min  = list.min
          val requiredList = list.filter(_ != min)
          selectSortHelper(requiredList, accumList ::: List.fill(list.length - requiredList.length)(min))
      }
    }
    val sort:List[Int]=selectSortHelper(array.toList)
    sort.toArray
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    for(i<- 1 until array.length) {
      for (j <- (i - 1) to 0 by -1) {
        if (array(j) > array(j + 1)) {
          val temp = array(j + 1)
          array(j + 1) = array(j)
          array(j) = temp
        }
      }
    }
    array
  }

}
