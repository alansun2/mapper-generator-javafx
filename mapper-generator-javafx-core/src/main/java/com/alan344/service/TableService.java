package com.alan344.service;

import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.service.driveservice.DriveFactory;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/9 16:48
 */
@Slf4j
@Service
public class TableService {
    @Resource
    private ColumnService columnService;

    @Resource
    private DriveFactory driveFactory;

    /**
     * 当展开datasource时加载tableItem，并将table写入文件
     *
     * @param dataSource 数据源的item
     */
    public List<Table> loadTables(DataSource dataSource) {
        List<Table> tables = this.pullTablesFromRemote(dataSource);
        if (!tables.isEmpty()) {
            //加载columns
            columnService.loadColumns(dataSource, tables);
            dataSource.setTables(tables);
            //写入文件
            this.downLoadToFileBatch(dataSource, tables);
        }
        return tables;
    }

    /**
     * 刷新tables
     *
     * @param dataSource 数据源
     */
    public List<Table> refreshTables(DataSource dataSource) {
        List<Table> tables = this.pullTablesFromRemote(dataSource);
        if (!tables.isEmpty()) {
            List<Table> existTables = dataSource.getTables();
            if (existTables != null && !existTables.isEmpty()) {
                Map<String, Table> tableNameTableMap = existTables.stream().collect(Collectors.toMap(Table::getTableName, table -> table));
                for (Table table : tables) {
                    if (tableNameTableMap.containsKey(table.getTableName())) {
                        Table table1 = tableNameTableMap.get(table.getTableName());
                        table.setReturnInsertId(table1.isReturnInsertId());
                        table.setInsert(table1.isInsert());
                        table.setCount(table1.isCount());
                        table.setUpdate(table1.isUpdate());
                        table.setDelete(table1.isDelete());
                        table.setSelect(table1.isSelect());
                        table.setUpdateExample(table1.isUpdateExample());
                        table.setDeleteExample(table1.isDeleteExample());
                        table.setSelectExample(table1.isSelectExample());
                    }
                }
            }

            //删除表文件夹
            this.deleteTableDirectory(dataSource);
            //表写入文件
            this.downLoadToFileBatch(dataSource, tables);
        }
        return tables;
    }

    /**
     * 从远程拉去表信息
     *
     * @param dataSource 数据源
     * @return {@link Table}
     */
    private List<Table> pullTablesFromRemote(DataSource dataSource) {
        return driveFactory.getDrive(dataSource.getDriveType()).getTables(dataSource);
    }

    /**
     * 从文件加表信息至pane
     *
     * @param dataSource 数据源
     */
    void loadTablesFromFile(DataSource dataSource) {
        File tableDirectory = BaseConstants.getTableDirectory(dataSource);
        if (!tableDirectory.exists()) {
            return;
        }

        File[] files = tableDirectory.listFiles();
        if (files == null || files.length <= 0) {
            return;
        }

        List<Table> tables = new ArrayList<>();
        try {
            for (File file : files) {
                Table table = JSONObject.parseObject(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString()), Table.class);
                tables.add(table);
            }
        } catch (IOException e) {
            log.error("加载tables文件失败", e);
            return;
        }

        columnService.loadColumnsFromFile(dataSource, tables);

        dataSource.setTables(tables);
    }

    /**
     * 删除table 同时删除columns
     *
     * @param dataSource 数据源信息
     */
    void deleteTables(DataSource dataSource) {
        this.deleteTableDirectory(dataSource);

        columnService.deleteColumnsAll(dataSource);
    }

    /**
     * 把tables信息记录到文件
     */
    @Async
    void downLoadToFileBatch(DataSource dataSource, List<Table> tables) {
        try {
            for (Table table : tables) {
                String tablesStr = JSON.toJSONString(table, true);
                FileUtils.writeStringToFile(BaseConstants.getTableFile(dataSource, table.getTableName()), tablesStr, StandardCharsets.UTF_8.toString());
            }
        } catch (IOException e) {
            log.error("写入表文件错误", e);
        }
    }

    /**
     * 把table信息记录到文件
     */
    @Async
    void downLoadToFileSingle(DataSource dataSource, Table table) {
        try {
            String tablesStr = JSON.toJSONString(table, true);
            FileUtils.writeStringToFile(BaseConstants.getTableFile(dataSource, table.getTableName()), tablesStr, StandardCharsets.UTF_8.toString());
        } catch (IOException e) {
            log.error("写入表文件错误", e);
        }
    }

    /**
     * 把 table 文件从磁盘删除
     */
    @Async
    void deleteTableFile(DataSource dataSource, String tableName) {
        try {
            FileUtils.forceDelete(BaseConstants.getTableFile(dataSource, tableName));
        } catch (IOException e) {
            log.error("删除表文件错误", e);
        }
    }

    /**
     * 把 table 目录从磁盘删除
     */
    @Async
    void deleteTableDirectory(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getTableDirectory(dataSource));
        } catch (IOException e) {
            log.error("删除表文件错误", e);
        }
    }

    /**
     * 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 table 配置（如 insert）文件重写
     */
    public void downLoadTableIfOverrideModify() {
        Map<String, Boolean> tableNameIsTableRecordMap = BaseConstants.tableNameSetUpTableRecordMap;
        if (!tableNameIsTableRecordMap.isEmpty()) {
            tableNameIsTableRecordMap.forEach((tableName, record) -> {
                this.deleteTableFile(BaseConstants.selectedDateSource, tableName);
                Table table = BaseConstants.selectedTableNameTableMap.get(tableName);
                this.downLoadToFileSingle(BaseConstants.selectedDateSource, table);
            });
            // 清空map,因为有多个数据源，一个导出结束后，用户可能还会选择别的数据源进行导出
            BaseConstants.tableNameSetUpTableRecordMap.clear();
        }
    }
}
