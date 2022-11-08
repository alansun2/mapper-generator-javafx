package com.alan344.utils;

import com.alan344.bean.Column;
import com.alan344.bean.Table;
import lombok.extern.slf4j.Slf4j;

import java.sql.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author AlanSun
 * @date 2021/6/20 0:34
 */
@Slf4j
public class DataSourceUtils {

    private static final String MYSQL = "MYSQL";
    private static final String ORACLE = "ORACLE";
    private static final String HANA = "HANA";
    private static final String SQLSERVER = "SQLSERVER";
    private static final String H2 = "H2";
    private static final String POSTGRESQL = "POSTGRESQL";
    private static final String ZENITH = "ZENITH";

    public static List<Table> getTables(Connection connection) {
        try {
            DatabaseMetaData metaData = connection.getMetaData();
            if (metaData == null) {
                return Collections.emptyList();
            }

            return getTables(metaData, connection.getCatalog(), connection.getSchema());
        } catch (Exception e) {
            log.error("error", e);
            throw new RuntimeException("获取数据库表异常");
        }
    }

    private static List<Table> getTables(DatabaseMetaData metaData, String catalog, String schema) throws SQLException {
        List<Table> list = new ArrayList<>();
        String[] types = {"TABLE"};
        ResultSet tables = metaData.getTables(catalog, schema, null, types);
        while (tables.next()) {
            String tableName = tables.getString("TABLE_NAME");
            Table table = new Table();
            table.setTableName(tableName);
            table.setRemark(tables.getString("REMARKS"));
            ResultSet primaryKeys = metaData.getPrimaryKeys(catalog, schema, tableName);
            while (primaryKeys.next()) {
                table.getPrimaryKey().add(primaryKeys.getString("COLUMN_NAME"));
            }
            list.add(table);
        }
        return list;
    }

    public static List<Column> getColumns(Connection connection, String tableName) {
        try {
            final DatabaseMetaData metaData = connection.getMetaData();
            return getColumns(metaData, connection.getCatalog(), connection.getSchema(), tableName);
        } catch (Exception e) {
            log.error("error", e);
            throw new RuntimeException("获取数据库字段异常");
        }
    }

    private static List<Column> getColumns(DatabaseMetaData metaData, String catalog, String schema, String tableName) {
        List<Column> list = new ArrayList<>();
        if (metaData == null) {
            return list;
        }

        try {
            ResultSet columns = metaData.getColumns(catalog, schema, tableName, "%");
            boolean supportsIsAutoIncrement = false;
            ResultSetMetaData rsmd = columns.getMetaData();
            int colCount = rsmd.getColumnCount();
            for (int i = 1; i <= colCount; i++) {
                if ("IS_AUTOINCREMENT".equals(rsmd.getColumnName(i))) {
                    supportsIsAutoIncrement = true;
                }
            }
            while (columns.next()) {
                Column column = new Column();
                column.setColumnName(columns.getString("COLUMN_NAME"));
                column.setType(columns.getString("TYPE_NAME"));
                column.setSize(columns.getInt("COLUMN_SIZE"));
                column.setRemark(columns.getString("REMARKS"));
                column.setNullable(columns.getInt("NULLABLE") == DatabaseMetaData.columnNullable);
                if (supportsIsAutoIncrement) {
                    column.setAutoIncr("YES".equals(columns.getString("IS_AUTOINCREMENT")));
                }
                list.add(column);
            }
            return list;
        } catch (Exception e) {
            log.error("error", e);
            throw new RuntimeException("获取数据库字段异常");
        }
    }
}
