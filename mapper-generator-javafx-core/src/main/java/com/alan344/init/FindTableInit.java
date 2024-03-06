package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.factory.TreeItemFactory;
import com.alan344.utils.CollectionUtils;
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
    public void addListener(TreeView<DataItem> treeViewDataSource, TextField tableFindTextField,
                            BorderPane borderPaneWrap) {
        tableFindTextField.textProperty().addListener((observable, oldValue, newValue) -> {
            if (oldValue != null && newValue != null) {
                this.filterTables(newValue, treeViewDataSource.getRoot(), newValue.length() < oldValue.length());
            }
        });

        tableFindTextField.setOnKeyPressed(this::escListener);

        treeViewDataSource.setOnKeyPressed(this::ctrlFListener);

        borderPaneWrap.setOnKeyPressed(this::ctrlFListener);
    }

    /**
     * filter tables when type tableName in the left BorderPane
     *
     * @param tableNamePrefix        tableNamePrefix
     * @param treeViewDataSourceRoot treeViewDataSourceRoot
     */
    private void filterTables(String tableNamePrefix, TreeItem<DataItem> treeViewDataSourceRoot, boolean isDelete) {
        System.out.println("tableNamePrefix = " + tableNamePrefix);
        tableNamePrefix = tableNamePrefix.toLowerCase().replaceAll("'", "");
        String tableNameFilterTem;
        if (tableNamePrefix.isEmpty()) {
            tableNameFilterTem = "";
        } else {
            final char[] charArray = tableNamePrefix.toCharArray();
            StringBuilder sb = new StringBuilder();
            for (char c : charArray) {
                sb.append(c).append(".*");
            }
            tableNameFilterTem = sb.toString();
        }

        System.out.println("tableNameFilterTem = " + tableNameFilterTem);
        final ObservableList<TreeItem<DataItem>> children = treeViewDataSourceRoot.getChildren();
        for (TreeItem<DataItem> dataSourceTreeItem : children) {
            DataSource dataSource = BaseConstants.allDataSources.get(dataSourceTreeItem);
            if (CollectionUtils.isNotEmpty(dataSource.getTables())) {
                if (tableNamePrefix.isEmpty()) {
                    dataSourceTreeItem.getChildren().clear();
                    for (Table filteredTable : dataSource.getTables()) {
                        TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(filteredTable, dataSourceTreeItem);
                        tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
                    }
                } else {
                    if (isDelete) {
                        dataSourceTreeItem.getChildren().clear();
                        List<Table> filteredTables =
                                dataSource.getTables().stream().filter(table -> table.getTableName().toLowerCase().matches(tableNameFilterTem)).toList();
                        for (Table filteredTable : filteredTables) {
                            TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(filteredTable,
                                    dataSourceTreeItem);
                            tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
                        }
                    }
                    final ObservableList<TreeItem<DataItem>> tableTreeItems = dataSourceTreeItem.getChildren();
                    tableTreeItems.removeIf(treeItem -> !treeItem.getValue().toString().toLowerCase().matches(tableNameFilterTem));
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
