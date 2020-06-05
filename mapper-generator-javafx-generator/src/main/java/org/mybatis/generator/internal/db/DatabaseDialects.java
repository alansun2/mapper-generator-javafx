/**
 *    Copyright 2006-2019 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package org.mybatis.generator.internal.db;

/**
 * Typesafe enum of known database dialects.
 *
 * @author Jeff Butler
 */
public enum DatabaseDialects {

    DB2("VALUES IDENTITY_VAL_LOCAL()"),
    MYSQL("SELECT LAST_INSERT_ID()"),
    SQLSERVER("SELECT SCOPE_IDENTITY()"),
    CLOUDSCAPE("VALUES IDENTITY_VAL_LOCAL()"),
    DERBY("VALUES IDENTITY_VAL_LOCAL()"),
    HSQLDB("CALL IDENTITY()"),
    SYBASE("SELECT @@IDENTITY"),
    DB2_MF("SELECT IDENTITY_VAL_LOCAL() FROM SYSIBM.SYSDUMMY1"),
    INFORMIX("select dbinfo('sqlca.sqlerrd1') from systables where tabid=1");

    private String identityRetrievalStatement;

    DatabaseDialects(String identityRetrievalStatement) {
        this.identityRetrievalStatement = identityRetrievalStatement;
    }

    public String getIdentityRetrievalStatement() {
        return identityRetrievalStatement;
    }

    /**
     * Gets the database dialect.
     *
     * @param database
     *            the database
     * @return the database dialect for the selected database. May return null if there is no known dialect for the
     *         selected db
     */
    public static DatabaseDialects getDatabaseDialect(String database) {
        DatabaseDialects returnValue = null;

        if ("DB2".equalsIgnoreCase(database)) {
            returnValue = DB2;
        } else if ("MySQL".equalsIgnoreCase(database)) {
            returnValue = MYSQL;
        } else if ("SqlServer".equalsIgnoreCase(database)) {
            returnValue = SQLSERVER;
        } else if ("Cloudscape".equalsIgnoreCase(database)) {
            returnValue = CLOUDSCAPE;
        } else if ("Derby".equalsIgnoreCase(database)) {
            returnValue = DERBY;
        } else if ("HSQLDB".equalsIgnoreCase(database)) {
            returnValue = HSQLDB;
        } else if ("SYBASE".equalsIgnoreCase(database)) {
            returnValue = SYBASE;
        } else if ("DB2_MF".equalsIgnoreCase(database)) {
            returnValue = DB2_MF;
        } else if ("Informix".equalsIgnoreCase(database)) {
            returnValue = INFORMIX;
        }

        return returnValue;
    }
}
