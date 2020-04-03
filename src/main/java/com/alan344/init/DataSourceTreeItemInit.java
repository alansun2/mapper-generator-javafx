package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.bean.component.FindViewTreeCell;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.service.DataSourceService;
import com.alan344.utils.KeyCombinationConstants;
import com.alan344.utils.TreeUtils;
import javafx.collections.ObservableList;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

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
    private RightListViewInit rightListViewInit;

    @Resource
    private MainController mainController;

    /**
     * 启动时加载数据文件
     */
    public void initLoadData(TreeItem<DataItem> treeItemDataSourceRoot) {
        // 从文件加载 dataSource 和 table 至 pane
        List<DataSource> dataSources = dataSourceService.loadDataSourceFromFile();
        // 加载 数据源 和 table
        this.loadDataSourceAndTable(dataSources, treeItemDataSourceRoot);
        // 如果只有一个数据源，则自动展开
        if (dataSources.size() == 1) {
            treeItemDataSourceRoot.getChildren().get(0).setExpanded(true);
        }
    }

    /**
     * 加载 数据源 和 table
     *
     * @param dataSources            数据源信息
     * @param treeItemDataSourceRoot treeView 的根节点
     */
    private void loadDataSourceAndTable(List<DataSource> dataSources, TreeItem<DataItem> treeItemDataSourceRoot) {
        if (!dataSources.isEmpty()) {
            for (DataSource dataSource : dataSources) {
                // 把 dataSource 放入 treeItemRoot
                TreeItem<DataItem> dataSourceTreeItem = TreeUtils.add2Tree(dataSource, treeItemDataSourceRoot);
                dataSourceTreeItem.setGraphic(new ImageView("/image/database.png"));
                // 设置 dataSource 展开监听，展开时清除之前别的数据源的缓存
                dataSourceTreeItem.expandedProperty().addListener((observable, oldValue, newValue) -> {
                    // 切换 ListView 内容
                    rightListViewInit.treeViewSwitch(dataSource);

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

                // 把 table 放入 dataSource TreeItem
                List<Table> tables = dataSource.getTables();
                if (tables != null && !tables.isEmpty()) {
                    tables.forEach(table -> {
                        TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                        tableTreeItem.setGraphic(new ImageView("/image/table.png"));
                    });
                }
            }
        }
    }


    private StringBuilder stringBuilder = new StringBuilder();

    /**
     * 给 DataSourceBorderPane 设置键盘监听，用于搜索 table
     */
    public void addListenOnDataSourceBorderPane(TreeView<DataItem> treeViewDataSource, Label tableFindLabel) {
        treeViewDataSource.setCellFactory(param -> new FindViewTreeCell(stringBuilder, treeViewDataSource));

        treeViewDataSource.setOnKeyReleased(event -> {
            KeyCode code = event.getCode();
            boolean match = true;
            // 字母 || 数字键 || 下划线 || 回退键
            if (code.isLetterKey() || code.isDigitKey()) {
                // 字母或数字键
                stringBuilder.append(code.getName().toLowerCase());
            } else if (KeyCombinationConstants.SHIFT_.match(event)) {
                stringBuilder.append("_");
            } else if (KeyCode.BACK_SPACE.equals(code)) {
                // 回退键
                if (stringBuilder.length() > 0) {
                    stringBuilder.deleteCharAt(stringBuilder.length() - 1);
                }
            } else {
                match = false;
            }

            if (match && stringBuilder.length() >= 0) {
                this.tt(stringBuilder.toString(), treeViewDataSource);
                if (stringBuilder.length() > 0) {
                    tableFindLabel.setVisible(true);
                    tableFindLabel.setText("Search for: " + stringBuilder.toString());
                } else {
                    tableFindLabel.setVisible(false);
                }
            }
        });

        // treeViewDataSource 失焦后清除
        treeViewDataSource.focusedProperty().addListener((observable, oldValue, newValue) -> {
            if (!newValue) {
                tableFindLabel.setText(null);
                tableFindLabel.setVisible(false);
                stringBuilder.setLength(0);
            }
        });
    }

    private void tt(String tableNamePrefix, TreeView<DataItem> treeViewDataSource) {
        final TreeItem<DataItem> treeViewDataSourceRoot = treeViewDataSource.getRoot();
        final ObservableList<TreeItem<DataItem>> dataSourceTreeItems = treeViewDataSourceRoot.getChildren();
        if (!dataSourceTreeItems.isEmpty()) {
            for (TreeItem<DataItem> dataSourceTreeItem : dataSourceTreeItems) {
//                if () {
//
//                }
            }
        }
    }
}
