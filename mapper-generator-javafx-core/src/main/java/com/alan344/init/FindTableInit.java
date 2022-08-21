package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.utils.TreeUtils;
import javafx.collections.ObservableList;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

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

    /**
     * filter tables when type tableName in the left BorderPane
     *
     * @param tableNamePrefix        tableNamePrefix
     * @param treeViewDataSourceRoot treeViewDataSourceRoot
     */
    private void filterTables(String tableNamePrefix, TreeItem<DataItem> treeViewDataSourceRoot, boolean isDelete) {
        final ObservableList<TreeItem<DataItem>> children = treeViewDataSourceRoot.getChildren();
        for (TreeItem<DataItem> dataSourceTreeItem : children) {
            if (dataSourceTreeItem.isExpanded()) {
                if (tableNamePrefix.length() > 0) {
                    if (isDelete) {
                        dataSourceTreeItem.getChildren().removeIf(treeItem -> true);
                        DataSource dataSource = BaseConstants.allDataSources.get(dataSourceTreeItem);
                        List<Table> filteredTables = dataSource.getTables().stream().filter(table -> table.getTableName().contains(tableNamePrefix)).collect(Collectors.toList());
                        for (Table filteredTable : filteredTables) {
                            TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(filteredTable, dataSourceTreeItem);
                            tableTreeItem.setGraphic(new ImageView("/image/table.png"));
                        }
                    }
                    final ObservableList<TreeItem<DataItem>> tableTreeItems = dataSourceTreeItem.getChildren();
                    tableTreeItems.removeIf(treeItem -> !treeItem.getValue().toString().contains(tableNamePrefix));

                } else {
                    dataSourceTreeItem.getChildren().removeIf(treeItem -> true);
                    DataSource dataSource = BaseConstants.allDataSources.get(dataSourceTreeItem);
                    for (Table filteredTable : dataSource.getTables()) {
                        TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(filteredTable, dataSourceTreeItem);
                        tableTreeItem.setGraphic(new ImageView("/image/table.png"));
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
