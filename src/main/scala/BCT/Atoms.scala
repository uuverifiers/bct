package bct

//
// Copyright 2018
// peter.backeman@it.uu.se
//


//
// Represents the atoms of the problem
//
case class Atom(val predicate : String, val args : List[Term]) {
  override def toString() = predicate + "(" + args.mkString(",") + ")"
  val terms = args.toSet
}
