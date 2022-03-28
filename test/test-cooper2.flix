/// test-cooper2.flix

namespace TestCooper2 {

use Cooper2/SQLTarget.SQLQuery.{SQLSelect};
use Cooper2/SQLTarget.SQLSelector.{SQLAs};
use Cooper2/SQLTarget.SQLExpr.{SQLFieldProjection, SQLConstant};
use Cooper2/SQLTarget.SQLConstant.{SQLInt, SQLString};

@test
def test01(): Int32 & Impure = 
    let q1 = SQLSelect(
        { selectors    = SQLAs(SQLFieldProjection("e1", "first_name"), "name") :: SQLAs(SQLFieldProjection("e1", "age_in_years"), "age") :: Nil
        , tableAliases = {tableName = "employee", variable = "e1"} :: Nil
        , whereExpr    = SQLConstant(SQLInt(1))      /// Makes valid but vacuous where clause
        }
    );
    let ans = Text/PrettyPrint.render(80, Cooper2/Pretty.pretty(q1));
    Console.printLine(ans);
    0

}
