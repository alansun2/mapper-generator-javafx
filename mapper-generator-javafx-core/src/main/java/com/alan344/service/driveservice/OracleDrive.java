package com.alan344.service.driveservice;

import com.alan344.bean.Column;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * @author AlanSun
 * @date 2020/12/2 16:46
 */
@Service
public class OracleDrive implements DriveInterface {

    @Autowired
    private BeanFactory beanFactory;

    /**
     * 获取表
     *
     * @param dataSource 数据源
     * @return 表
     */
    @Override
    public List<Table> getTables(DataSource dataSource) {
        JdbcTemplate jdbcTemplate = beanFactory.getBean(dataSource.toString(), JdbcTemplate.class);
        List<Table> tables = new ArrayList<>();
        List<String> tableNames = jdbcTemplate.query("SELECT TABLE_NAME FROM user_tables", (rs, i) -> rs.getString(1));
        if (!tableNames.isEmpty()) {
            //把table填入dataSourceTreeItem
            tableNames.forEach(tableName -> {
                Table table = new Table();
                table.setTableName(tableName);
                tables.add(table);
            });
        }

        return tables;
    }

    /**
     * 获取某一表的所有字段
     *
     * @param dataSource 数据源
     * @param tableName  表
     * @return 表字段
     */
    @Override
    public List<Column> getColumn(DataSource dataSource, String tableName) {
        JdbcTemplate jdbcTemplate = beanFactory.getBean(dataSource.toString(), JdbcTemplate.class);
        List<Column> columns = new ArrayList<>();
        jdbcTemplate.query("select COLUMN_NAME, DATA_TYPE \n" +
                "FROM user_tab_columns\n" +
                "WHERE TABLE_NAME='" + tableName + "'\n" +
                "ORDER BY COLUMN_NAME", (rs, rowNum) -> {
            Column column = new Column();
            column.setColumnName(rs.getString(1));
            column.setType(rs.getString(2));
            columns.add(column);
            return column;
        });

        return columns;
    }
}
