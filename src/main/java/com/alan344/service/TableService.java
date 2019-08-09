package com.alan344.service;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.utils.TreeUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.zaxxer.hikari.HikariDataSource;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
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
import java.util.Collection;
import java.util.List;

/**
 * @author AlanSun
 * @date 2019/8/9 16:48
 */
@Slf4j
@Service
public class TableService {
    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Autowired
    private BeanFactory beanFactory;

    /**
     * 当展开datasource时加载tableItem，并将table写入文件
     *
     * @param dataSourceTreeItem 数据源的item
     */
    void expandTables(TreeItem<DataItem> dataSourceTreeItem) {
        ObservableList<TreeItem<DataItem>> children = dataSourceTreeItem.getChildren();

        //当dataSourceTreeItem下只有一个item，并且该item是之前填充的item时才进行拉去table的操作
        if (children.size() == 1 && ((DataSource) children.get(0).getValue()).getHost() == null) {
            DataSource dataSource = ((DataSource) dataSourceTreeItem.getValue());
            jdbcTemplate.setDataSource(beanFactory.getBean(dataSource.toString(), HikariDataSource.class));

            try {
                List<String> tableNames = jdbcTemplate.query("SHOW TABLES", (rs, i) -> rs.getString(1));
                if (!tableNames.isEmpty()) {
                    //删除用来填充的item
                    dataSourceTreeItem.getChildren().remove(0);

                    List<Table> tables = new ArrayList<>();
                    //把table填入dataSourceTreeItem
                    tableNames.forEach(tableName -> {
                        Table table = new Table();
                        table.setTableName(tableName);
                        TreeUtils.add2Tree(table, dataSourceTreeItem);

                        tables.add(table);
                    });
                    this.downLoadToFile(dataSource, tables);
                }
            } catch (Exception e) {
                log.error("数据源有问题" + dataSource, e);
            }
        }
    }

    /**
     * 从文件加表信息至pane
     *
     * @param treeItemDataSource treeItemDataSource
     */
    boolean loadTablesFromFile(TreeItem<DataItem> treeItemDataSource) {
        File file1 = new File(BaseConstants.MG_TABLE_HOME);
        if (!file1.exists()) {
            return false;
        }
        Collection<File> files = FileUtils.listFiles(file1, null, false);
        if (files.isEmpty()) {
            return false;
        }
        for (File file : files) {
            try {
                List<Table> tables = JSONArray.parseArray(FileUtils.readFileToString(file), Table.class);
                tables.forEach(table -> TreeUtils.add2Tree(table, treeItemDataSource));
            } catch (IOException e) {
                log.error("加载tables文件失败", e);
                return false;
            }
        }
        return true;

    }

    /**
     * 把tables信息记录到文件
     */
    @Async
    public void downLoadToFile(DataSource dataSource, List<Table> tables) throws IOException {
        String tablesStr = JSON.toJSONString(tables);

        FileUtils.writeStringToFile(new File(BaseConstants.MG_TABLE_HOME + dataSource.toString()), tablesStr);
    }

    /**
     * 把datasource文件从磁盘删除
     */
    @Async
    public void deleteTableFile(DataSource dataSource) {
        try {
            FileUtils.forceDelete(new File(BaseConstants.MG_TABLE_HOME + dataSource.toString()));
        } catch (IOException e) {
            log.error("删除表文件错误", e);
        }
    }

    public void setListView(List<Table> tables, ListView<AnchorPane> tableListView) {
        ObservableList<AnchorPane> anchorPanes = FXCollections.observableArrayList();
        tableListView.setItems(anchorPanes);
        for (Table table : tables) {
            CheckBox returnId = new CheckBox("insert返回id");
            CheckBox insert = new CheckBox("insert");
            CheckBox count = new CheckBox("count");
            CheckBox update = new CheckBox("update");
            CheckBox delete = new CheckBox("delete");
            CheckBox select = new CheckBox("select");
            HBox hBox = new HBox(10, returnId, insert, count, update, delete, select);
            hBox.setAlignment(Pos.CENTER);

            BorderPane borderPane = new BorderPane(hBox);

            Label tableNameLabel = new Label(table.getTableName());
            tableNameLabel.setPrefWidth(150d);
//            AnchorPane.setLeftAnchor(tableNameLabel, 10d);
            double v = tableNameLabel.getLayoutX() + tableNameLabel.getPrefWidth() + 10d;
            AnchorPane.setLeftAnchor(borderPane, v);
            AnchorPane anchorPane = new AnchorPane(tableNameLabel, borderPane);
            anchorPane.setPrefHeight(100);

            anchorPanes.add(anchorPane);
        }
    }
}
