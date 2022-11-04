package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.controller.DataSourceController;
import com.alan344.controller.MainController;
import com.alan344.service.ColumnService;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Assert;
import com.alan344.utils.CollectionUtils;
import com.alan344.utils.TreeUtils;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2020/4/2 17:56
 */
@Service
public class DataSourceTreeViewInit {
    @Resource
    private ColumnService columnService;
    @Resource
    private DataSourceService dataSourceService;
    @Resource
    private TableService tableService;
    @Resource
    private MybatisListViewInit mybatisListViewInit;
    @Resource
    private MainController mainController;
    @Resource
    private DataSourceController dataSourceController;

    /**
     * treeView init
     */
    public void treeViewInit(TreeView<DataItem> treeViewDataSource) {
        // 设置鼠标释放事件
        treeViewDataSource.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
            if (event.getButton() == MouseButton.SECONDARY) {
                // 右键释放
                //item is selected - this prevents fail when clicking on empty space
                if (selectedItems != null) {
                    ContextMenu contextMenu;
                    //open context menu on current screen position
                    if (selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {
                        MenuItem connectMenuItem = new MenuItem("连接");
                        connectMenuItem.setGraphic(new FontIcon("unil-cloud-data-connection:16:BLUE"));
                        connectMenuItem.setOnAction(actionEvent -> this.initDataSourceItem(treeViewDataSource));
                        MenuItem updateMenuItem = new MenuItem("编辑");
                        updateMenuItem.setGraphic(new FontIcon("unil-file-edit-alt:16:ORANGE"));
                        updateMenuItem.setOnAction(event1 -> updateDataSource(treeViewDataSource));
                        MenuItem exportMenuItem = new MenuItem("导出");
                        exportMenuItem.setGraphic(new ImageView("/image/export-datasource@16.png"));
                        exportMenuItem.setOnAction(event1 -> this.export(treeViewDataSource));
                        MenuItem refreshMenuItem = new MenuItem("刷新");
                        refreshMenuItem.setGraphic(new ImageView("/image/refresh@16.png"));
                        refreshMenuItem.setOnAction(event1 -> refreshDataSource(treeViewDataSource));
                        MenuItem deleteMenuItem = new MenuItem("删除数据源");
                        deleteMenuItem.setGraphic(new ImageView("/image/delete@16.png"));
                        deleteMenuItem.setOnAction(this::deleteDataSource);

                        contextMenu = new ContextMenu(connectMenuItem, updateMenuItem, exportMenuItem, refreshMenuItem, deleteMenuItem);
                    } else {
                        // 只有一个导出按钮
                        MenuItem exportMenuItem = new MenuItem("导出");
                        exportMenuItem.setGraphic(new ImageView("/image/export-datasource@16.png"));
                        exportMenuItem.setOnAction(event1 -> export(treeViewDataSource));

                        contextMenu = new ContextMenu(exportMenuItem);
                    }
                    // 放入  contextMenu
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
        if (value instanceof DataSource) {
            final DataSource dataSource = (DataSource) value;
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
                TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new ImageView("/image/table.png"));
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
                //选中数据源时的导出
                ObservableList<TreeItem<DataItem>> children = dataItemTreeItem.getChildren();
                tables = new ArrayList<>();
                if (!children.isEmpty()) {
                    children.forEach(itemTreeItem -> tables.add(((Table) itemTreeItem.getValue())));
                }

                dataSource = (DataSource) dataItemTreeItem.getValue();
            } else {
                //单独选中table的导出
                Table table = (Table) dataItemTreeItem.getValue();
                tables = Collections.singletonList(table);

                dataSource = ((DataSource) dataItemTreeItem.getParent().getValue());
            }
        } else {
            //选中多个table的导出
            tables = new ArrayList<>();
            TreeItem<DataItem> lastParent = null;
            for (TreeItem<DataItem> selectedItem : selectedItems) {
                DataItem dataItem = selectedItem.getValue();
                if (dataItem instanceof Table) {
                    final Table table = (Table) dataItem;
                    if (lastParent == null) {
                        lastParent = selectedItem.getParent();
                        dataSource = ((DataSource) selectedItem.getParent().getValue());
                    } else {
                        Assert.isTrue(lastParent == selectedItem.getParent(), "请选择一个数据源的表导出", NodeConstants.primaryStage);
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
        BaseConstants.selectedTableNameTableMap = tables.stream().collect(Collectors.toMap(Table::getTableName, o -> o));

        // 选中的 dataSource
        BaseConstants.selectedDateSource = dataSource;

        // 用于当再不同的 dataSource 之间切换时，保留原来的 tables
        BaseConstants.dataSourceTableVBoxListMap.put(BaseConstants.selectedDateSource, vBoxes);

        BaseConstants.dataSourceTableListMap.put(BaseConstants.selectedDateSource, tables.stream().collect(Collectors.toMap(Table::getTableName, table -> table)));

        // 如果没有字段，则从远程加载
        tables.forEach(table -> columnService.reloadColumnsIfNotNull(table));

        //show rightBorderTopHBox
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
        TreeItem<DataItem> dataItemTreeItem = mainController.getTreeViewDataSource().getSelectionModel().getSelectedItem();

        // 从根节点删除数据源
        mainController.getTreeItemDataSourceRoot().getChildren().remove(dataItemTreeItem);

        DataSource dataSource = (DataSource) dataItemTreeItem.getValue();

        // 关闭右边的 Border 展示
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
                TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                tableTreeItem.setGraphic(new ImageView("/image/table.png"));
            });
        }

        // 关闭右边的 Border 展示
        this.rightBorderShowClose(dataSource);
    }

    /**
     * 跟新数据源
     *
     * @param treeViewDataSource 被选中的数据源
     */
    private void updateDataSource(TreeView<DataItem> treeViewDataSource) {
        TreeItem<DataItem> dataSourceTreeItem = treeViewDataSource.getSelectionModel().getSelectedItem();
        dataSourceTreeItem.setExpanded(false);
        DataSource dataSource = (DataSource) dataSourceTreeItem.getValue();
        dataSourceTreeItem.getChildren().clear();
        dataSourceController.openDataSourceSetUp(NodeConstants.primaryStage, dataSource);
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
