/** Type checking with variability.
  */
package de.uni_saarland.cs.se

import Constant.*
import Type.*
import VExpression.*

import cafesat.api.Formulas.Formula
import cafesat.api.{Formulas, Solver}

/** Variability context as in lecture slides 74.
  */
case class VariabilityContext(formula: Formula) {

  /** Make equality consider logical equality of formulas.
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: VariabilityContext =>
        Solver.solveForSatisfiability(!(formula iff other.formula)) match {
          case Some(_) => false
          case None    => true
        }
      case _ => false
    }
  }

  override def toString: String = formula.toString
}

object VariabilityContext {

  /** Creates an empty variability context.
    */
  def emptyContext(): VariabilityContext = new VariabilityContext(Formulas.True)

  /** Allow implicit conversion from formulas to `VariabilityContext`.
    */
  given formulaToVarCtx: Conversion[Formula, VariabilityContext] =
    VariabilityContext(_)
}

/** Type alias for type context type */
type VTypeContext = TypeContext[Identifier, VType]

/** Type alias for context (= variability context + type context) type */
type VContext = (VariabilityContext, VTypeContext)

/** Type alias for result type */
type VResult = TypeCheckResult[VExpression, VType, VContext]

object VariabilityTypeChecker {

  /** Type-check a single expression.
    */
  def checkType(
      expr: VExpression,
      context: VContext = createContext()
  ): VResult = {
    new VariabilityTypeChecker().checkType(expr, context)
  }

  /** Simplify creation of variability context + type context.
    */
  def createContext(
      variabilityContext: VariabilityContext =
        VariabilityContext.emptyContext(),
      typeContext: VTypeContext = TypeContext()
  ): VContext = (variabilityContext, typeContext)
}

/** Type checker implementation for the language with variability.
  */
class VariabilityTypeChecker extends TypeChecker[VExpression, VType, VContext] {

  override def checkType(expr: VExpression, context: VContext): VResult = {
    expr match {
      case expr: Const => expr.c match {
        case True => new Success(VType(BoolTy -> Formulas.True)) 
        case False => new Success(VType(BoolTy -> Formulas.True))
        case const: Num => new Success(VType(NumTy -> Formulas.True)) 
        case null => new Failure(expr,context,"ERROR!")
      };
      case expr: Id => expr.id match{
        case cont if(expr.id == context) => new Success(VType(BoolTy -> Formulas.True))
        case _ => new Failure(expr,context,"ERROR!")
      };
      case expr: Smaller => expr.rhs match{
       case i if(expr.rhs.getClass.toString == "int") =>  new Success(VType(BoolTy -> Formulas.True))
       case j if(expr.lhs.getClass.toString == "int") =>  new Success(VType(BoolTy -> Formulas.True))
       case _ => new Failure(expr,context,"ERROR!")
      };
      case expr: If => expr.condition match{
        case success if(expr.condition == Const(True) || expr.condition == Const(False))  =>  new Success(VType(BoolTy -> Formulas.True))
        case _ => new Failure(expr,context,"ERROR!")
      };
     case expr: Let => expr.variable match{
        case success if(context == expr.variable) => new Success(VType(BoolTy -> Formulas.True))
        case _ => new Failure(expr,context,"ERROR!")
      }
     case Choice(presenceCondition,trueChoice,falseChoice) => presenceCondition match{
       //TODO
     }
      case _ => new Failure(expr,context,"ERROR!")
  }
}
}
