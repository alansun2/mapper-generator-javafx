package com.alan344.service;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.TreeUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
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
import java.util.List;

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
    void loadTables(TreeItem<DataItem> dataSourceTreeItem) {
        ObservableList<TreeItem<DataItem>> children = dataSourceTreeItem.getChildren();
        Table emptyTable = (Table) children.get(0).getValue();
        //当dataSourceTreeItem下只有一个item，并且该item是之前填充的item时才进行拉去table的操作
        if (emptyTable.getTableName() == null) {
            DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
            JdbcTemplate jdbcTemplate = beanFactory.getBean(dataSource.toString(), JdbcTemplate.class);

            try {
                List<String> tableNames = jdbcTemplate.query("SHOW TABLES", (rs, i) -> rs.getString(1));
                if (!tableNames.isEmpty()) {
                    //删除用来填充的item
                    children.remove(0);

                    List<Table> tables = new ArrayList<>();
                    //把table填入dataSourceTreeItem
                    tableNames.forEach(tableName -> {
                        Table table = new Table();
                        table.setTableName(tableName);
                        TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                        tableTreeItem.setGraphic(new ImageView("/image/table.png"));

                        tables.add(table);
                    });
                    //写入文件
                    this.downLoadToFile(dataSource, tables);

                    //加载columns
                    columnService.loadColumns(dataSource, tables);
                }
            } catch (Exception e) {
                log.error("数据源有问题" + dataSource, e);
            }
        }
    }

    /**
     * 刷新tables
     *
     * @param dataSourceTreeItem 数据源 treeItem
     */
    public void refreshTables(TreeItem<DataItem> dataSourceTreeItem) {
        DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
        this.deleteTable(dataSource);

        ObservableList<TreeItem<DataItem>> children = dataSourceTreeItem.getChildren();
        children.remove(0, children.size());
        //下面个没啥用，填充table，让界面看前来有一个下拉箭头，可能会在loadTables方法中删除该item
        TreeUtils.add2Tree(new Table(), dataSourceTreeItem);
        this.loadTables(dataSourceTreeItem);
    }

    /**
     * 从文件加表信息至pane
     *
     * @param treeItemDataSource treeItemDataSource
     */
    boolean loadTablesFromFile(TreeItem<DataItem> treeItemDataSource) {
        DataSource dataSource = (DataSource) treeItemDataSource.getValue();
        File tableFile = BaseConstants.getTableFile(dataSource);
        if (!tableFile.exists()) {
            return false;
        }
        List<Table> tables;
        try {
            tables = JSONArray.parseArray(FileUtils.readFileToString(tableFile, StandardCharsets.UTF_8.toString()), Table.class);
            tables.forEach(table -> {
                TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, treeItemDataSource);
                tableTreeItem.setGraphic(new ImageView("/image/table.png"));
            });
            columnService.loadColumnsFromFile(dataSource, tables);
        } catch (IOException e) {
            log.error("加载tables文件失败", e);
            return false;
        }

        return true;
    }

    /**
     * 删除table
     *
     * @param dataSource 数据源信息
     */
    void deleteTable(DataSource dataSource) {
        this.deleteTableFile(dataSource);

        columnService.deleteColumns(dataSource);
    }

    /**
     * 把tables信息记录到文件
     */
    @Async
    void downLoadToFile(DataSource dataSource, List<Table> tables) throws IOException {
        String tablesStr = JSON.toJSONString(tables, true);
        FileUtils.writeStringToFile(BaseConstants.getTableFile(dataSource), tablesStr, StandardCharsets.UTF_8.toString());
    }

    /**
     * 把table文件从磁盘删除
     */
    @Async
    void deleteTableFile(DataSource dataSource) {
        try {
            FileUtils.forceDelete(BaseConstants.getTableFile(dataSource));
        } catch (IOException e) {
            log.error("删除表文件错误", e);
        }
    }
}
