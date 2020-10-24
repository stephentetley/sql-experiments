/// Cooper.flix

use Cooper/SqlFragment.Query.{Select};
use Cooper/SqlFragment.SelectFromWhere.{SelectFromWhere};
use Cooper/SqlFragment.TableAlias.{TableAlias};
use Cooper/SqlFragment.Selector.{As};
use Cooper/SqlFragment.Expr.{FieldProjection, Constant};
use Cooper/SqlFragment.Constant.{Int, String};
use Cooper/SqlFragment.{ppQuery};

def main(): Unit & Impure = 
    let q1 = Select(
        SelectFromWhere(
          As(FieldProjection("e1", "first_name"), "name") :: As(FieldProjection("e1", "age_in_years"), "age") :: Nil
        , TableAlias("employee", "e1") :: Nil
        , Constant(Int(1))      /// Makes valid but vacuous where clause
        )
    );
    let ans = Text/PrettyPrint.prettyPrint(80, ppQuery(q1));
    Console.printLine(ans)