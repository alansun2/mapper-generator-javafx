package com.alan344.service;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.TreeUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
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
    @Autowired
    private BeanFactory beanFactory;

    @Autowired
    private ColumnService columnService;

    /**
     * 当展开datasource时加载tableItem，并将table写入文件
     *
     * @param dataSourceTreeItem 数据源的item
     */
    public void loadTables(TreeItem<DataItem> dataSourceTreeItem) {
        ObservableList<TreeItem<DataItem>> children = dataSourceTreeItem.getChildren();
        Table emptyTable = (Table) children.get(0).getValue();
        //当dataSourceTreeItem下只有一个item，并且该item是之前填充的item时才进行拉去table的操作
        if (emptyTable.getTableName() == null) {
            //删除用来填充的item
            children.remove(0);

            List<Table> tables = this.pullTablesFromRemote(dataSourceTreeItem);

            DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
            dataSource.setTables(tables);
            //写入文件
            this.downLoadToFileBatch(dataSource, tables);
            //加载columns
            columnService.loadColumns(dataSource, tables);
        }
    }

    /**
     * 刷新tables
     *
     * @param dataSourceTreeItem 数据源 treeItem
     */
    public void refreshTables(TreeItem<DataItem> dataSourceTreeItem) {
        DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
        this.deleteTableOnly(dataSource);

        ObservableList<TreeItem<DataItem>> children = dataSourceTreeItem.getChildren();
        children.remove(0, children.size());
        List<Table> tables = this.pullTablesFromRemote(dataSourceTreeItem);
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
        dataSource.setTables(tables);

        //写入文件
        this.downLoadToFileBatch(dataSource, tables);
    }

    /**
     * 从远程拉去表信息
     *
     * @param dataSourceTreeItem 数据源 TreeView
     * @return {@link Table}
     */
    private List<Table> pullTablesFromRemote(TreeItem<DataItem> dataSourceTreeItem) {
        DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
        JdbcTemplate jdbcTemplate = beanFactory.getBean(dataSource.toString(), JdbcTemplate.class);
        List<Table> tables = new ArrayList<>();
        List<String> tableNames = jdbcTemplate.query("SHOW TABLES", (rs, i) -> rs.getString(1));
        if (!tableNames.isEmpty()) {
            //把table填入dataSourceTreeItem
            tableNames.forEach(tableName -> {
                Table table = new Table();
                table.setTableName(tableName);
                TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new ImageView("/image/table.png"));

                tables.add(table);
            });
        }

        return tables;
    }

    /**
     * 从文件加表信息至pane
     *
     * @param dataSource 数据源
     */
    List<Table> loadTablesFromFile(DataSource dataSource) {
        File tableDirectory = BaseConstants.getTableDirectory(dataSource);
        if (!tableDirectory.exists()) {
            return Collections.emptyList();
        }

        File[] files = tableDirectory.listFiles();
        if (files == null || files.length <= 0) {
            return Collections.emptyList();
        }

        List<Table> tables = new ArrayList<>();
        try {
            for (File file : files) {
                Table table = JSONObject.parseObject(FileUtils.readFileToString(file, StandardCharsets.UTF_8.toString()), Table.class);
                tables.add(table);
            }
        } catch (IOException e) {
            log.error("加载tables文件失败", e);
            return Collections.emptyList();
        }

        columnService.loadColumnsFromFile(dataSource, tables);
        return tables;
    }

    /**
     * 删除table 同时删除columns
     *
     * @param dataSource 数据源信息
     */
    void deleteTable(DataSource dataSource) {
        this.deleteTableDirectory(dataSource);

        columnService.deleteColumnsDirectory(dataSource);
    }

    /**
     * 删除table
     *
     * @param dataSource 数据源信息
     */
    private void deleteTableOnly(DataSource dataSource) {
        this.deleteTableDirectory(dataSource);
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
     * 把tables信息记录到文件
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
     * 把table文件从磁盘删除
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
     * 把table文件从磁盘删除
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
     * 导出时，如果 tableNameIsOverrideRecodeMap 不为空，则把 columns 文件重写
     */
    public void downLoadTableIfOverrideModify() {
        Map<String, Boolean> tableNameIsTableRecordMap = BaseConstants.tableNameIsTableRecordMap;
        if (!tableNameIsTableRecordMap.isEmpty()) {
            tableNameIsTableRecordMap.forEach((tableName, record) -> {
                this.deleteTableFile(BaseConstants.selectedDateSource, tableName);
                Table table = BaseConstants.selectedTableNameTableMap.get(tableName);
                this.downLoadToFileSingle(BaseConstants.selectedDateSource, table);
            });
        }

        //清空map,因为有多个数据源，一个导出结束后，用户可能还会选择别的数据源进行导出
        BaseConstants.tableNameIsTableRecordMap.clear();
    }
}
