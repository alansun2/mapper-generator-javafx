package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.service.DataSourceService;
import com.alan344.utils.TreeUtils;
import javafx.collections.ObservableList;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2020/4/2 17:45
 * <p>
 * DataSource 初始化
 */
@Service
public class DataSourceTreeItemInit {
    @Resource
    private DataSourceService dataSourceService;

    @Resource
    private MainController mainController;

    @Resource
    private TableTextFieldListener tableTextFieldListener;

    /**
     * 启动时加载数据文件
     */
    public void initLoadData(TreeItem<DataItem> treeItemDataSourceRoot) {
        // 从文件加载 dataSource 和 table 至 pane
        List<DataSource> dataSources = dataSourceService.loadDataSourceFromFile();

        // 加载 数据源 和 table
        dataSources.forEach(dataSource -> this.addExpandListenerForDataSource(dataSource, treeItemDataSourceRoot));

        // 如果只有一个数据源，则自动展开
        if (dataSources.size() == 1) {
            treeItemDataSourceRoot.getChildren().get(0).setExpanded(true);
        }
    }

    /**
     * 把 DataSource 放入根节点，并设置监听
     *
     * @param dataSource             dataSource
     * @param treeItemDataSourceRoot 根节点
     */
    public void addExpandListenerForDataSource(DataSource dataSource, TreeItem<DataItem> treeItemDataSourceRoot) {
        TreeItem<DataItem> dataSourceTreeItem = TreeUtils.add2Tree(dataSource, treeItemDataSourceRoot);
        dataSourceTreeItem.setGraphic(new ImageView("/image/database.png"));
        // 设置 dataSource 展开监听，展开时清除之前别的数据源的缓存
        dataSourceTreeItem.expandedProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                // 设置 table 可以多选
                mainController.getTreeViewDataSource().getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

                if (BaseConstants.selectedDateSource != null) {
                    BaseConstants.tableNameIsOverrideRecodeMap.clear();
                    BaseConstants.tableNameSetUpTableRecordMap.clear();
                }
            } else {
                // false : 全部都未展开； true: 有展开的
                boolean hasExpanded = false;
                final ObservableList<TreeItem<DataItem>> children = treeItemDataSourceRoot.getChildren();
                if (children != null && !children.isEmpty()) {
                    for (TreeItem<DataItem> dataSourceItem : children) {
                        hasExpanded = dataSourceItem.isExpanded();
                        if (hasExpanded) {
                            break;
                        }
                    }
                }

                // 如果没有展开的 item，则改为单选
                if (!hasExpanded) {
                    // 设置 table 单选
                    mainController.getTreeViewDataSource().getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
                }
            }
        });

        // package Tables and inert them to the DataSourceTreeItem
        this.packageTablesAndInertDataSourceTreeItem(dataSource.getTables(), dataSourceTreeItem);

        // put dataSource into allDataSource
        BaseConstants.allDataSources.put(dataSourceTreeItem, dataSource);
    }

    /**
     * package Tables and inert them to the DataSourceTreeItem
     *
     * @param tables             tables
     * @param dataSourceTreeItem dataSourceTreeItem
     */
    private void packageTablesAndInertDataSourceTreeItem(List<Table> tables, TreeItem<DataItem> dataSourceTreeItem) {
        if (tables != null && !tables.isEmpty()) {
            tables.forEach(table -> {
                TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new ImageView("/image/table.png"));
            });
        }
    }

    /**
     * 给 DataSourceBorderPane 设置键盘监听，用于搜索 table
     */
    public void addListenOnDataSourceBorderPane(TreeView<DataItem> treeViewDataSource) {
        TextField tableFindTextField = mainController.getTableFindTextField();
        tableFindTextField.textProperty().addListener((observable, oldValue, newValue) -> {
            if (oldValue != null && newValue != null) {
                this.filterTables(newValue, treeViewDataSource.getRoot(), oldValue.length() > newValue.length());
            }
        });

        tableFindTextField.setOnKeyPressed(tableTextFieldListener::escListener);

        treeViewDataSource.setOnKeyPressed(tableTextFieldListener::ctrlFListener);
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
                        List<Table> filteredTables = dataSource.getTables().stream().filter(table -> table.getTableName().startsWith(tableNamePrefix)).collect(Collectors.toList());
                        for (Table filteredTable : filteredTables) {
                            TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(filteredTable, dataSourceTreeItem);
                            tableTreeItem.setGraphic(new ImageView("/image/table.png"));
                        }
                    }
                    final ObservableList<TreeItem<DataItem>> tableTreeItems = dataSourceTreeItem.getChildren();
                    tableTreeItems.removeIf(treeItem -> !treeItem.getValue().toString().startsWith(tableNamePrefix));

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
}
