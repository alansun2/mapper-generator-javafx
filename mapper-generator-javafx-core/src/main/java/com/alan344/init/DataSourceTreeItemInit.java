package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.factory.TreeItemFactory;
import com.alan344.service.DataSourceService;
import javafx.collections.ObservableList;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TreeItem;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

/**
 * @author AlanSun
 * @since 2020/4/2 17:45
 * <p>
 * DataSource 初始化
 */
@Service
public class DataSourceTreeItemInit {
    @Autowired
    private DataSourceService dataSourceService;

    @Autowired
    private MainController mainController;

    /**
     * 启动时加载数据文件
     */
    public void initLoadData(TreeItem<DataItem> treeItemDataSourceRoot) {
        // 从文件加载 dataSource 和 table 至 pane
        List<DataSource> dataSources = dataSourceService.loadDataSourceFromFile();

        // 加载 数据源 和 table
        dataSources.forEach(dataSource -> this.addExpandListenerForDataSource(dataSource, treeItemDataSourceRoot));
    }

    /**
     * 把 DataSource 放入根节点，并设置监听
     *
     * @param dataSource             dataSource
     * @param treeItemDataSourceRoot 根节点
     */
    public void addExpandListenerForDataSource(DataSource dataSource, TreeItem<DataItem> treeItemDataSourceRoot) {
        TreeItem<DataItem> dataSourceTreeItem = TreeItemFactory.add2Tree(dataSource, treeItemDataSourceRoot);
        dataSourceTreeItem.setGraphic(new FontIcon("unil-database:16:#6E6E6F"));
        // 设置 dataSource 展开监听，展开时清除之前别的数据源的缓存
        dataSourceTreeItem.expandedProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue) {
                dataSourceTreeItem.setGraphic(new FontIcon("unil-database:16:#388ce0"));
                // 设置 table 可以多选
                mainController.getTreeViewDataSource().getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

                if (BaseConstants.selectedDateSource != null) {
                    BaseConstants.tableNameIsOverrideRecodeMap.clear();
                    BaseConstants.tableNameSetUpTableRecordMap.clear();
                }
            } else {
                // false : 全部都未展开； true: 有展开的
                final ObservableList<TreeItem<DataItem>> children = treeItemDataSourceRoot.getChildren();
                Optional.ofNullable(children).map(treeItems -> treeItems.stream().anyMatch(TreeItem::isExpanded))
                        .ifPresent(hasExpanded -> {
                            // 如果没有展开的 item，则改为单选
                            if (!hasExpanded) {
                                // 设置 table 单选
                                mainController.getTreeViewDataSource().getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
                            }
                        });
            }
        });

        // put dataSource into allDataSource
        BaseConstants.allDataSources.put(dataSourceTreeItem, dataSource);
    }
}
