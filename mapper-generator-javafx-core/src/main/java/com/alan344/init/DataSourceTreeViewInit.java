package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.controller.DataSourceSetupController;
import com.alan344.controller.MainController;
import com.alan344.factory.TreeItemFactory;
import com.alan344.service.ColumnService;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Assert;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.NameUtils;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @since 2020/4/2 17:56
 */
@Service
public class DataSourceTreeViewInit {
    @Autowired
    private ColumnService columnService;
    @Autowired
    private DataSourceService dataSourceService;
    @Autowired
    private TableService tableService;
    @Autowired
    private MybatisListViewInit mybatisListViewInit;
    @Autowired
    private MainController mainController;
    @Autowired
    private DataSourceTreeItemInit dataSourceTreeItemInit;
    @Autowired
    private DataSourceSetupController dataSourceSetupController;

    private final Map<Integer, ContextMenu> contextMenuMap = new HashMap<>();

    /**
     * treeView init
     */
    public void treeViewInit(TreeView<DataItem> treeViewDataSource) {
        // 设置鼠标释放事件
        treeViewDataSource.addEventHandler(MouseEvent.MOUSE_PRESSED, event -> {
            ObservableList<TreeItem<DataItem>> selectedItems =
                    treeViewDataSource.getSelectionModel().getSelectedItems();
            if (event.getButton() == MouseButton.SECONDARY) {
                // 右键释放
                // item is selected - this prevents fail when clicking on empty space
                if (selectedItems != null) {
                    ContextMenu contextMenu;
                    // open context menu on current screen position
                    if (selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {
                        final TreeItem<DataItem> selectedDataSourceItem =
                                treeViewDataSource.getSelectionModel().getSelectedItem();
                        if (CollectionUtils.isNotEmpty(selectedDataSourceItem.getChildren())) {
                            contextMenu = contextMenuMap.computeIfAbsent(1, integer -> {
                                MenuItem closeMenuItem = new MenuItem("关闭");
                                closeMenuItem.setGraphic(new FontIcon("unil-padlock:16:GRAY"));
                                closeMenuItem.setOnAction(event1 -> this.close(treeViewDataSource));
                                MenuItem updateMenuItem = new MenuItem("编辑");
                                updateMenuItem.setGraphic(new FontIcon("unil-file-edit-alt:16:ORANGE"));
                                updateMenuItem.setOnAction(event1 -> updateDataSource(treeViewDataSource));
                                MenuItem exportMenuItem = new MenuItem("导出所有");
                                exportMenuItem.setGraphic(new FontIcon("unil-export:16:GREEN"));
                                exportMenuItem.setOnAction(event1 -> this.export(treeViewDataSource));
                                MenuItem copyMenuItem = new MenuItem("复制");
                                copyMenuItem.setGraphic(new FontIcon("unil-copy:16:BLUE"));
                                copyMenuItem.setOnAction(event1 -> this.copy(treeViewDataSource));
                                MenuItem refreshMenuItem = new MenuItem("刷新");
                                refreshMenuItem.setGraphic(new FontIcon("unis-refresh:16:GRAY"));
                                refreshMenuItem.setOnAction(event1 -> refreshDataSource(treeViewDataSource));
                                MenuItem deleteMenuItem = new MenuItem("删除数据源");
                                deleteMenuItem.setGraphic(new FontIcon("unil-times-circle:16:RED"));
                                deleteMenuItem.setOnAction(this::deleteDataSource);
                                return new ContextMenu(closeMenuItem, updateMenuItem, exportMenuItem, copyMenuItem,
                                        refreshMenuItem, deleteMenuItem);
                            });
                        } else {
                            contextMenu = contextMenuMap.computeIfAbsent(2, integer -> {
                                MenuItem connectMenuItem = new MenuItem("连接");
                                connectMenuItem.setGraphic(new FontIcon("unil-cloud-data-connection:16:BLUE"));
                                connectMenuItem.setOnAction(actionEvent -> this.initDataSourceItem(treeViewDataSource));
                                MenuItem updateMenuItem = new MenuItem("编辑");
                                updateMenuItem.setGraphic(new FontIcon("unil-file-edit-alt:16:ORANGE"));
                                updateMenuItem.setOnAction(event1 -> updateDataSource(treeViewDataSource));
                                MenuItem copyMenuItem = new MenuItem("复制");
                                copyMenuItem.setGraphic(new FontIcon("unil-copy:16:BLUE"));
                                copyMenuItem.setOnAction(event1 -> this.copy(treeViewDataSource));
                                MenuItem deleteMenuItem = new MenuItem("删除数据源");
                                deleteMenuItem.setGraphic(new FontIcon("unil-times-circle:16:RED"));
                                deleteMenuItem.setOnAction(this::deleteDataSource);
                                return new ContextMenu(connectMenuItem, updateMenuItem, copyMenuItem, deleteMenuItem);
                            });
                        }
                    } else {
                        // 只有一个导出按钮
                        contextMenu = contextMenuMap.computeIfAbsent(3, integer -> {
                            MenuItem exportMenuItem = new MenuItem("导出");
                            exportMenuItem.setGraphic(new FontIcon("unil-export:16:GREEN"));
                            exportMenuItem.setOnAction(event1 -> export(treeViewDataSource));
                            return new ContextMenu(exportMenuItem);
                        });
                    }
                    // 放入 contextMenu
                    treeViewDataSource.setContextMenu(contextMenu);
                }
            } else if (event.getButton() == MouseButton.PRIMARY
                       && event.getClickCount() == 1
                       && CollectionUtils.isNotEmpty(treeViewDataSource.getSelectionModel().getSelectedItems())) {
                // 左键释放时。切换 listView
                final TreeItem<DataItem> selectedDataSourceItem = treeViewDataSource.getSelectionModel().getSelectedItem();
                DataSource dataSource;
                if (selectedDataSourceItem.getValue() instanceof DataSource) {
                    dataSource = (DataSource) selectedDataSourceItem.getValue();
                } else {
                    dataSource = (DataSource) selectedDataSourceItem.getParent().getValue();
                }

                mybatisListViewInit.treeViewSwitch(dataSource);
                // 清除原来的数据
                BaseConstants.tableNameSetUpTableRecordMap.clear();
            } else if (event.getButton() == MouseButton.PRIMARY
                       && event.getClickCount() == 2
                       && CollectionUtils.isNotEmpty(treeViewDataSource.getSelectionModel().getSelectedItems())) {
                final TreeItem<DataItem> selectedDataSourceItem = treeViewDataSource.getSelectionModel().getSelectedItem();
                if (selectedDataSourceItem.getValue() instanceof DataSource) {
                    // 双击数据源展开
                    this.initDataSourceItem(treeViewDataSource);
                } else {
                    this.export(treeViewDataSource);
                }
            }
        });
    }

    private void initDataSourceItem(TreeView<DataItem> treeViewDataSource) {
        final TreeItem<DataItem> selectedDataSourceItem = treeViewDataSource.getSelectionModel().getSelectedItem();
        final DataItem value = selectedDataSourceItem.getValue();
        if (value instanceof DataSource dataSource) {
            if (!selectedDataSourceItem.getChildren().isEmpty()) {
                return;
            }
            if (!selectedDataSourceItem.isExpanded()) {
                tableService.loadTables(dataSource);
                final List<Table> tables = dataSource.getTables();
                if (CollectionUtils.isEmpty(tables)) {
                    final Table table = new Table();
                    table.setTableName("(empty)");
                    tables.add(table);
                } else {
                    // package Tables and inert them to the DataSourceTreeItem
                    this.packageTablesAndInertDataSourceTreeItem(dataSource.getTables(), selectedDataSourceItem);
                }
                selectedDataSourceItem.setExpanded(true);
            }
        }
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
                TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
            });
        }
    }

    /*------------------------------------TreeView ContextMenu--------------------------------------------------------*/

    /**
     * 导出
     */
    private void export(TreeView<DataItem> treeViewDataSource) {
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        List<Table> tables;
        DataSource dataSource = null;
        if (selectedItems.size() == 1) {
            TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
            if (dataItemTreeItem.getValue() instanceof DataSource) {
                // 选中数据源时的导出
                ObservableList<TreeItem<DataItem>> children = dataItemTreeItem.getChildren();
                tables = new ArrayList<>();
                if (!children.isEmpty()) {
                    children.forEach(itemTreeItem -> tables.add(((Table) itemTreeItem.getValue())));
                }

                dataSource = (DataSource) dataItemTreeItem.getValue();
            } else {
                // 单独选中table的导出
                Table table = (Table) dataItemTreeItem.getValue();
                tables = Collections.singletonList(table);

                dataSource = ((DataSource) dataItemTreeItem.getParent().getValue());
            }
        } else {
            // 选中多个table的导出
            tables = new ArrayList<>();
            TreeItem<DataItem> lastParent = null;
            for (TreeItem<DataItem> selectedItem : selectedItems) {
                DataItem dataItem = selectedItem.getValue();
                if (dataItem instanceof Table table) {
                    if (lastParent == null) {
                        lastParent = selectedItem.getParent();
                        dataSource = ((DataSource) selectedItem.getParent().getValue());
                    } else {
                        Assert.isTrue(lastParent == selectedItem.getParent(), "请选择一个数据源的表导出",
                                NodeConstants.primaryStage);
                    }

                    tables.add(table);
                }
            }

            Assert.isTrue(!tables.isEmpty(), "请选择一个数据源的表导出", NodeConstants.primaryStage);
        }
        // 清空当前checkBoxVBox
        BaseConstants.selectedCheckBoxVBox.clear();

        // 把选中要导出的表在右边的listView展示
        ObservableList<VBox> vBoxes = mybatisListViewInit.setListView(tables);

        // 选中的表放入map
        BaseConstants.selectedTableNameTableMap = tables.stream().collect(Collectors.toMap(Table::getTableName,
                o -> o));

        // 选中的 dataSource
        BaseConstants.selectedDateSource = dataSource;

        // 用于当再不同的 dataSource 之间切换时，保留原来的 tables
        BaseConstants.dataSourceTableVBoxListMap.put(BaseConstants.selectedDateSource, vBoxes);

        BaseConstants.dataSourceTableListMap.put(BaseConstants.selectedDateSource,
                tables.stream().collect(Collectors.toMap(Table::getTableName, table -> table)));

        // 如果没有字段，则从远程加载
        tables.forEach(table -> columnService.reloadColumnsIfNotNull(table));

        // show rightBorderTopHBox
        final BorderPane borderPane1 = NodeConstants.borderPane1;
        if (!borderPane1.isVisible() && !borderPane1.isManaged()) {
            borderPane1.setVisible(true);
            borderPane1.setManaged(true);
        }

        // 展示第一页
        NodeConstants.borderPaneWrap.setCenter(borderPane1);

        // 清空map,因为有多个数据源，一个导出结束后，用户可能还会选择别的数据源进行导出
        BaseConstants.tableNameSetUpTableRecordMap.clear();
    }

    /**
     * 删除数据源
     */
    private void deleteDataSource(ActionEvent actionEvent) {
        // 选中的数据源
        TreeItem<DataItem> dataItemTreeItem =
                mainController.getTreeViewDataSource().getSelectionModel().getSelectedItem();

        // 从根节点删除数据源
        mainController.getTreeItemDataSourceRoot().getChildren().remove(dataItemTreeItem);

        DataSource dataSource = (DataSource) dataItemTreeItem.getValue();

        // 取消右边的 Border 展示
        this.rightBorderShowClose(dataSource);

        // 删除对应的文件
        dataSourceService.deleteDataSource(dataSource);

        // 删除全局的记录
        BaseConstants.allDataSources.remove(dataItemTreeItem);
    }

    /**
     * 刷新数据源下的 table
     * <p>
     * 对table
     */
    private void refreshDataSource(TreeView<DataItem> treeViewDataSource) {
        TreeItem<DataItem> dataSourceTreeItem = treeViewDataSource.getSelectionModel().getSelectedItem();
        DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
        List<Table> tables = tableService.refreshTables(dataSource);
        if (!tables.isEmpty()) {
            ObservableList<TreeItem<DataItem>> children = dataSourceTreeItem.getChildren();
            children.remove(0, children.size());
            tables.forEach(table -> {
                TreeItem<DataItem> tableTreeItem = TreeItemFactory.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new FontIcon("unim-table:16:BLACK"));
            });
        }

        // 关闭右边的 Border 展示
        this.rightBorderShowClose(dataSource);
    }

    /**
     * 关闭数据源
     *
     * @param treeViewDataSource 被选中的数据源
     */
    private void copy(TreeView<DataItem> treeViewDataSource) {
        TreeItem<DataItem> dataSourceTreeItem = treeViewDataSource.getSelectionModel().getSelectedItem();
        final DataItem value = dataSourceTreeItem.getValue();
        final DataSource dataSourceCopy = ((DataSource) value).copy();
        dataSourceCopy.setSort(dataSourceService.getMaxSort() + 1);
        final List<DataItem> dataItemList =
                treeViewDataSource.getRoot().getChildren().stream().map(TreeItem::getValue).toList();
        dataSourceCopy.setConfigName(NameUtils.generatorName(dataSourceCopy.getConfigName(), dataItemList));
        // 添加数据源
        dataSourceService.addDataSource(dataSourceCopy);

        // 把 dataSource 放入 treeItemRoot
        dataSourceTreeItemInit.addExpandListenerForDataSource(dataSourceCopy, treeViewDataSource.getRoot());
    }

    /**
     * 关闭数据源
     *
     * @param treeViewDataSource 被选中的数据源
     */
    private void close(TreeView<DataItem> treeViewDataSource) {
        TreeItem<DataItem> dataSourceTreeItem = treeViewDataSource.getSelectionModel().getSelectedItem();
        dataSourceTreeItem.setExpanded(false);
        dataSourceTreeItem.setGraphic(new FontIcon("unil-database:16:#6E6E6F"));
        dataSourceTreeItem.getChildren().clear();

        // 关闭右边的 Border 展示
        this.rightBorderShowClose(((DataSource) dataSourceTreeItem.getValue()));
    }

    /**
     * 跟新数据源
     *
     * @param treeViewDataSource 被选中的数据源
     */
    private void updateDataSource(TreeView<DataItem> treeViewDataSource) {
        TreeItem<DataItem> dataSourceTreeItem = treeViewDataSource.getSelectionModel().getSelectedItem();
        DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
        dataSourceSetupController.openDataSourceSetUp(NodeConstants.primaryStage, treeViewDataSource, dataSource);
        this.close(treeViewDataSource);
    }

    /**
     * 关闭右边的 Border 展示
     *
     * @param dataSource 数据源
     */
    private void rightBorderShowClose(DataSource dataSource) {
        if (BaseConstants.selectedDateSource == null || BaseConstants.selectedDateSource == dataSource) {
            final BorderPane borderPane1 = NodeConstants.borderPane1;
            borderPane1.setVisible(false);
            borderPane1.setManaged(false);
        }
    }
}
