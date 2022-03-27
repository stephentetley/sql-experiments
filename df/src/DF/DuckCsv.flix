/*
 * Copyright 2022 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */




namespace DF/DuckCsv {

    use Functor.{<$>};
    use Applicative.{<*>};

    use BasicDb.DbMonad;
    use BasicDb.RowEvaluator;

    pub type alias DuckDbColumn = 
        { columnName :: String
        , ordinalPosition :: Int32
        , dataType :: String
        , isNullable :: Bool
        }


    pub def loadCsv(r: {tableName :: String}, path: String): DbMonad[Unit] = 
        use BasicDb/DbMonad.{flatMap, point};
        let _       = BasicDb/DbMonad.execute("DROP TABLE IF EXISTS ${r.tableName};");
        let* _      = BasicDb/DbMonad.liftAction(_ -> DF/Utils.sleep(5i64));
        let* _      = BasicDb/DbMonad.execute("CREATE TABLE IF NOT EXISTS ${r.tableName} AS SELECT * FROM read_csv_auto('${path}');");
        point()

    pub def queryCsv(path: String): DbMonad[List[(String, String)]] = 
        use BasicDb/DbMonad.{flatMap, point};
        let* stmt   = BasicDb/DbMonad.createStatement();
        let* rs     = BasicDb/DbMonad.executeQuery(stmt, "SELECT * FROM read_csv_auto('${path}');");
        let* xs     = BasicDb/DbMonad.allRows(rs, evalDatalogsRow());
        let* _      = BasicDb/DbMonad.rsClose(rs);
        point(xs)

    pub def query1(r: {tableName :: String}): DbMonad[List[(String, String)]] = 
        BasicDb/Reader.allRows(evalDatalogsRow(), "SELECT * FROM ${r.tableName};")

    
    def evalDatalogsRow(): RowEvaluator[(String, String)] = 
        use BasicDb/RowEvaluator.{getStringAt};
        Applicative.product(getStringAt(0), getStringAt(1))


    pub def getDuckDbColumns(r: {tableName :: String}): DbMonad[List[DuckDbColumn]] = 
        BasicDb/Reader.allRows(evalDuckDbColumn(), getDuckDbColumnsQuery(r.tableName))
        

    def getDuckDbColumnsQuery(tableName: String): String = 
        String.unlines(
                "SELECT" 
            :: "t1.column_name,"
            :: "t1.ordinal_position,"
            :: "t1.data_type,"
            :: "t1.is_nullable"
            :: "FROM information_schema.columns AS t1"
            :: "WHERE table_schema = 'main'"
            :: "AND table_name = '${tableName}';"
            :: Nil
        )


    def evalDuckDbColumn(): RowEvaluator[DuckDbColumn] = 
        use BasicDb/RowEvaluator.{getStringAt, getInt32At};
        ((x1, x2, x3, x4) -> 
            { columnName        = x1
            , ordinalPosition   = x2
            , dataType          = x3
            , isNullable        = String.toUpperCase(x4) == "YES"
            })
            <$> getStringAt(0)
            <*> getInt32At(1)
            <*> getStringAt(2)
            <*> getStringAt(3)

}
