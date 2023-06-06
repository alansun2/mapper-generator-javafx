package com.alan344.init;

import cn.hutool.core.util.ReUtil;
import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.factory.TreeItemFactory;
import javafx.collections.ObservableList;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author AlanSun
 * @date 2020/9/9 10:40
 * <p>
 * table 查询增加监听
 */
@Component
public class FindTableInit {
    @Resource
    private MainController mainController;

    /**
     * 给 DataSourceBorderPane 设置键盘监听，用于搜索 table
     */
    public void addListener(TreeView<DataItem> treeViewDataSource, TextField tableFindTextField, BorderPane borderPaneWrap) {
        tableFindTextField.textProperty().addListener((observable, oldValue, newValue) -> {
            if (oldValue != null && newValue != null) {
                this.filterTables(newValue, treeViewDataSource.getRoot(), oldValue.length() > newValue.length());
            }
        });

        tableFindTextField.setOnKeyPressed(this::escListener);

        treeViewDataSource.setOnKeyPressed(this::ctrlFListener);

        borderPaneWrap.setOnKeyPressed(this::ctrlFListener);
    }

    private String tableNameFilter = "";

    private String tableNamePrefixCache = "";

    private long lastUpdateTime = 0;

    /**
     * filter tables when type tableName in the left BorderPane
     *
     * @param tableNamePrefix        tableNamePrefix
     * @param treeViewDataSourceRoot treeViewDataSourceRoot
     */
    private void filterTables(String tableNamePrefix, TreeItem<DataItem> treeViewDataSourceRoot, boolean isDelete) {
        System.out.println("a = " + (System.currentTimeMillis() - lastUpdateTime));
        tableNamePrefix = tableNamePrefix.toLowerCase().replaceAll("'", "");
        String tableNameFilterInit = tableNameFilter;
        final long now = System.currentTimeMillis();
        if (!isDelete) {
            final long l = now - lastUpdateTime;
            System.out.println("l = " + l);
            if (l > 500) {
                tableNameFilterInit = tableNameFilterInit + tableNamePrefix.replace(tableNamePrefixCache, "") + ".*";
            } else {
                tableNameFilterInit = tableNameFilterInit + tableNamePrefix.replace(tableNamePrefixCache, "");
            }
        } else {
            tableNameFilterInit = ReUtil.delLast(tableNamePrefixCache.replace(tableNamePrefix, ""), tableNameFilterInit);
            tableNameFilterInit = tableNameFilterInit.replace(".*.*", ".*");
        }

        tableNamePrefixCache = tableNamePrefix;
        tableNameFilter = tableNameFilterInit;
        lastUpdateTime = now;

        if (!tableNameFilterInit.endsWith(".*")) {
            tableNameFilterInit = tableNameFilterInit + ".*";
        }
        System.out.println("tableNameFilterInit = " + tableNameFilterInit);
        final ObservableList<TreeItem<DataItem>> children = treeViewDataSourceRoot.getChildren();
        for (TreeItem<DataItem> dataSourceTreeItem : children) {
            if (dataSourceTreeItem.isExpanded()) {
                if (tableNamePrefix.length() > 0) {
                    if (isDelete) {
                        dataSourceTreeItem.getChildren().removeIf(treeItem -> true);
                        DataSource dataSource = BaseConstants.allDataSources.get(dataSourceTreeItem);
                        String finalTableNameFilterInit = tableNameFilterInit;
                        List<Table> filteredTables = dataSource.getTables().stream().filter(table -> table.getTableName().matches(finalTableNameFilterInit)).toList();
                        for (Table filteredTable : filteredTables) {
                            TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(filteredTable, dataSourceTreeItem);
                            tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
                        }
                    }
                    final ObservableList<TreeItem<DataItem>> tableTreeItems = dataSourceTreeItem.getChildren();
                    String finalTableNameFilterInit1 = tableNameFilterInit;
                    tableTreeItems.removeIf(treeItem -> !treeItem.getValue().toString().matches(finalTableNameFilterInit1));


                } else {
                    dataSourceTreeItem.getChildren().removeIf(treeItem -> true);
                    DataSource dataSource = BaseConstants.allDataSources.get(dataSourceTreeItem);
                    for (Table filteredTable : dataSource.getTables()) {
                        TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(filteredTable, dataSourceTreeItem);
                        tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
                    }
                }
            }
        }
    }

    public void escListener(KeyEvent event) {
        KeyCode code = event.getCode();
        if (KeyCode.ESCAPE.equals(code)) {
            mainController.getTableFindTextField().setText("");
            mainController.getTreeViewDataSource().requestFocus();
        }
    }

    public void ctrlFListener(KeyEvent event) {
        KeyCode code = event.getCode();
        // ctrl + F
        if (KeyCode.F.equals(code)) {
            if (event.isControlDown()) {
                mainController.getTableFindTextField().requestFocus();
            }
        } else if (KeyCode.ESCAPE.equals(code)) {
            mainController.getTableFindTextField().setText("");
        }
    }
}
