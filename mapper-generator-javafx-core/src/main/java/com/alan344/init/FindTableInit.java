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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author AlanSun
 * @since 2020/9/9 10:40
 * <p>
 * table 查询增加监听
 */
@Component
public class FindTableInit {
    @Autowired
    private MainController mainController;

    /**
     * 给 DataSourceBorderPane 设置键盘监听，用于搜索 table
     */
    public void addListener(TreeView<DataItem> treeViewDataSource, TextField tableFindTextField,
                            BorderPane borderPaneWrap) {
        tableFindTextField.textProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue != null) {
                this.filterTables(newValue, treeViewDataSource.getRoot());
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
    private void filterTables(String tableNamePrefix, TreeItem<DataItem> treeViewDataSourceRoot) {
        final String finalTableNamePrefix = tableNamePrefix.toLowerCase().replaceAll("'", "");

        final ObservableList<TreeItem<DataItem>> children = treeViewDataSourceRoot.getChildren();
        for (TreeItem<DataItem> dataSourceTreeItem : children) {
            if (!dataSourceTreeItem.isExpanded()) {
                continue;
            }
            DataSource dataSource = BaseConstants.allDataSources.get(dataSourceTreeItem);
            if (CollectionUtils.isNotEmpty(dataSource.getTables())) {
                dataSourceTreeItem.getChildren().clear();

                final List<Table> tablesToShow;
                if (finalTableNamePrefix.isEmpty()) {
                    tablesToShow = dataSource.getTables();
                } else {
                    tablesToShow = dataSource.getTables().stream()
                            .filter(table -> this.isSubsequence(finalTableNamePrefix, table.getTableName().toLowerCase()))
                            .toList();
                }

                for (Table table : tablesToShow) {
                    TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(table, dataSourceTreeItem);
                    tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
                }
            }
        }
    }

    private boolean isSubsequence(String sub, String target) {
        int subIndex = 0, targetIndex = 0;
        while (subIndex < sub.length() && targetIndex < target.length()) {
            if (sub.charAt(subIndex) == target.charAt(targetIndex)) {
                subIndex++;
            }
            targetIndex++;
        }
        return subIndex == sub.length();
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