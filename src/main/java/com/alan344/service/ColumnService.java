package com.alan344.service;

import com.alan344.bean.Column;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author AlanSun
 * @date 2019/8/15 12:10
 */
@Slf4j
@Service
public class ColumnService {
    @Autowired
    private BeanFactory beanFactory;

    /**
     * 加载columns
     *
     * @param dataSource 数据源
     * @param tables     表
     */
    Map<String, List<Column>> loadColumns(DataSource dataSource, List<Table> tables) {
        Map<String, List<Column>> tableNameColumnsMap = new HashMap<>();
        for (Table table : tables) {
            String tableName = table.getTableName();
            List<Column> columns = this.getColumns(dataSource, tableName);
            table.setColumns(columns);
            tableNameColumnsMap.put(tableName, columns);
        }

        //写入文件
        this.downLoadColumnsToFile(dataSource, tableNameColumnsMap);

        return tableNameColumnsMap;
    }

    /**
     * 获取表的字段
     *
     * @param tableName tableName
     * @return 字段数组
     */
    public List<Column> getColumns(DataSource dataSource, String tableName) {
        JdbcTemplate jdbcTemplate = beanFactory.getBean(dataSource.toString(), JdbcTemplate.class);
        List<Column> columns = new ArrayList<>();
        jdbcTemplate.query("DESC " + tableName, (rs, rowNum) -> {
            Column column = new Column();
            column.setColumnName(rs.getString(1));
            column.setType(rs.getString(2));
            columns.add(column);
            return column;
        });

        return columns;
    }

    /**
     * columns 写入文件
     *
     * @param dataSource          数据源
     * @param tableNameColumnsMap columnsMap
     */
    @Async
    void downLoadColumnsToFile(DataSource dataSource, Map<String, List<Column>> tableNameColumnsMap) {
        File columnsFile = BaseConstants.getColumnsFile(dataSource);
        String tableNameColumnsMapStr = JSONObject.toJSONString(tableNameColumnsMap, true);
        try {
            FileUtils.writeStringToFile(columnsFile, tableNameColumnsMapStr);
        } catch (IOException e) {
            log.error("columns download 失败");
        }
    }

    /**
     * 从文件加载字段信息
     *
     * @param dataSource 数据源
     */
    void loadColumnsFromFile(DataSource dataSource, List<Table> tables) {
        File columnsFile = BaseConstants.getColumnsFile(dataSource);
        if (!columnsFile.exists()) {
            return;
        }

        try {
            String tableNameColumnsMapStr = FileUtils.readFileToString(columnsFile);

            Map<String, List<Column>> tableNameColumnsMap = JSONObject.parseObject(tableNameColumnsMapStr, new TypeReference<Map<String, List<Column>>>() {
            });
            tables.forEach(table -> table.setColumns(tableNameColumnsMap.get(table.getTableName())));
        } catch (IOException e) {
            log.error("加载columns文件失败", e);
        }
    }

    /**
     * 删除table
     *
     * @param dataSource 数据源信息
     */
    void deleteColumns(DataSource dataSource) {
        this.deleteColumnFile(dataSource);
    }

    /**
     * 把columns文件从磁盘删除
     */
    @Async
    void deleteColumnFile(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getColumnsFile(dataSource));
        } catch (IOException e) {
            log.error("删除字段文件错误", e);
        }
    }
}
