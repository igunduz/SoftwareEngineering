/** Type checking without variability.
  */
package de.uni_saarland.cs.se

import Constant.*
import Expression.*
import Type.*

/** Type context as in lecture slides 71/74.
  *
  * @tparam IdT
  *   type used for variables
  * @tparam TypeT
  *   the types of the language
  */
case class TypeContext[IdT, TypeT] private (mapping: Map[IdT, TypeT]) {

  /** Create an extended copy of this type context that sets the type for the
    * given variable.
    */
  def withVar(id: IdT, value: TypeT): TypeContext[IdT, TypeT] = {
    new TypeContext(mapping updated (id, value))
  }

  /** Get the type for a given variable.
    */
  def typeForVar(id: IdT): Option[TypeT] = mapping.get(id)

  override def toString: String = {
    mapping.toSeq
      .map({ case (id: IdT, t: TypeT) =>
        s"($id: $t)"
      })
      .mkString("\n")
  }
}

object TypeContext {

  /** Magic function so that we can write 
    * `TypeContext(("x", VType(BoolTy -> * A)))` instead of
    * `new TypeContext(Map("x", VType(BoolTy -> A)))`.
    */
  def apply[IdT, TypeT](elems: (IdT, TypeT)*): TypeContext[IdT, TypeT] =
    new TypeContext(Map.from(elems))
}

/** Type alias for context type, i.e., the type context. */
type Context = TypeContext[Identifier, Type]

/** Type alias for result type. */
type Result = TypeCheckResult[Expression, Type, Context]

object SimpleTypeChecker {

  /** Type-check a single expression.
    */
  def checkType(
      expr: Expression,
      context: Context = TypeContext()
  ): Result = {
    new SimpleTypeChecker().checkType(expr, context)
  }
}

/** Type checker implementation for the language without variability.
  */
class SimpleTypeChecker extends TypeChecker[Expression, Type, Context] {

  override def checkType(expr: Expression, context: Context): Result = {
    expr match {
      case expr: Const => expr.c match {
        case True =>  new Success(BoolTy) 
        case False => new Success(BoolTy) 
        case constant: Num =>  new Success(NumTy)
        case _ => new Failure(expr,context,"ERROR!")
      };
      case expr: Id => expr.id match{
        case cont if(expr.id == context) => new Success(BoolTy)
        case _ => new Failure(expr,context,"ERROR!")
      };
      case expr: Smaller => expr.rhs match{
       case i if(expr.rhs.getClass.toString == "int" && expr.lhs.getClass.toString == "int") =>  new Success(BoolTy)
       case _ => new Failure(expr,context,"ERROR!")
      };
      case expr: If => expr.condition match{
        case t if(expr.condition.toString == "true") =>  new Success(BoolTy)
        case f if(expr.condition.toString == "false") => new Failure(expr,context,"ERROR!")
        //case thenExpr if(expr.thenExpr.toString == "true" || expr.thenExpr.toString == "false") => new Success(BoolTy)
        //case elseExpr if(expr.elseExpr.toString == "true"  || expr.elseExpr.toString == "false") => new Success(BoolTy)
        case _ => new Failure(expr,context,"ERROR!")
      };
      case expr: Let => expr.varValue match{
        case some if(context != expr.varValue) => new Success(BoolTy)
        case _ => new Failure(expr,context,"ERROR!")
      }
      //case _ => new Failure(expr,context,"ERROR!")
}
}
}
  
