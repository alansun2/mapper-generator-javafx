package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.service.ColumnService;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Assert;
import com.alan344.utils.TreeUtils;
import javafx.collections.ObservableList;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import org.springframework.context.ApplicationContext;
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
    private RightListViewInit rightListViewInit;

    @Resource
    private MainController mainController;

    @Resource
    private ApplicationContext applicationContext;

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

                        MenuItem exportMenuItem = new MenuItem("导出");
                        exportMenuItem.setGraphic(new ImageView("/image/export-datasource@16.png"));
                        exportMenuItem.setOnAction(event1 -> export(treeViewDataSource));
                        MenuItem refreshMenuItem = new MenuItem("刷新");
                        refreshMenuItem.setGraphic(new ImageView("/image/refresh@16.png"));
                        refreshMenuItem.setOnAction(event1 -> refreshDataSource(treeViewDataSource));
                        MenuItem deleteMenuItem = new MenuItem("删除数据源");
                        deleteMenuItem.setGraphic(new ImageView("/image/delete@16.png"));
                        deleteMenuItem.setOnAction(event1 -> deleteDataSource());

                        contextMenu = new ContextMenu(exportMenuItem, refreshMenuItem, deleteMenuItem);
                    } else {
                        // 只有一个到处按钮
                        MenuItem exportMenuItem = new MenuItem("导出");
                        exportMenuItem.setGraphic(new ImageView("/image/export-datasource@16.png"));
                        exportMenuItem.setOnAction(event1 -> export(treeViewDataSource));

                        contextMenu = new ContextMenu(exportMenuItem);
                    }
                    // 放入  contextMenu
                    treeViewDataSource.setContextMenu(contextMenu);
                }
            } else if (event.getButton() == MouseButton.PRIMARY && selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {
                // 左键释放时 && 选择的时数据源。切换 listView
                final TreeItem<DataItem> selectedDataSourceItem = treeViewDataSource.getSelectionModel().getSelectedItem();
                final DataSource dataSource = (DataSource) selectedDataSourceItem.getValue();
                rightListViewInit.treeViewSwitch(dataSource);
            }
        });
    }

    /*------------------------------------TreeView ContextMenu--------------------------------------------------------*/

    /**
     * 全部导出
     */
    private void export(TreeView<DataItem> treeViewDataSource) {
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        List<Table> tables;
        if (selectedItems.size() == 1) {
            TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
            if (dataItemTreeItem.getValue() instanceof DataSource) {
                //选中数据源时的导出
                ObservableList<TreeItem<DataItem>> children = dataItemTreeItem.getChildren();
                tables = new ArrayList<>();
                if (!children.isEmpty()) {
                    children.forEach(itemTreeItem -> tables.add(((Table) itemTreeItem.getValue())));
                }

                BaseConstants.selectedDateSource = (DataSource) dataItemTreeItem.getValue();
            } else {
                //单独选中table的导出
                Table table = (Table) dataItemTreeItem.getValue();
                tables = Collections.singletonList(table);

                BaseConstants.selectedDateSource = ((DataSource) dataItemTreeItem.getParent().getValue());
            }
        } else {
            //选中多个table的导出
            tables = new ArrayList<>();
            TreeItem<DataItem> lastParent = null;
            for (TreeItem<DataItem> selectedItem : selectedItems) {
                DataItem dataItem = selectedItem.getValue();
                if (dataItem instanceof Table) {
                    if (lastParent == null) {
                        lastParent = selectedItem.getParent();
                    } else {
                        Assert.isTrue(lastParent == selectedItem.getParent(), "请选择一个数据源的表导出", applicationContext.getBean(Stage.class));
                    }

                    Table table = (Table) dataItem;
                    tables.add(table);
                }
            }

            BaseConstants.selectedDateSource = ((DataSource) selectedItems.get(0).getParent().getValue());
        }
        // 清空当前checkBoxVBox
        BaseConstants.selectedCheckBoxVBox.clear();

        // 把选中要导出的表在右边的listView展示
        rightListViewInit.setListView(tables, mainController.getVBoxListView());
        // 选中的表放入map
        BaseConstants.selectedTableNameTableMap = tables.stream().collect(Collectors.toMap(Table::getTableName, o -> o));
        // 用于当再不同的 dataSource 之间切换时，保留原来的 tables
        BaseConstants.dataSourceTableListMap.put(BaseConstants.selectedDateSource, tables);

        // 如果没有字段，则从远程加载
        tables.forEach(table -> columnService.reloadColumnsIfNotNull(table));

        //show rightBorderTopHBox
        final BorderPane borderPane1 = mainController.getBorderPane1();
        if (!borderPane1.isVisible() && !borderPane1.isManaged()) {
            borderPane1.setVisible(true);
            borderPane1.setManaged(true);
        }
    }

    /**
     * 删除数据源
     */
    private void deleteDataSource() {
        // 选中的数据源
        TreeItem<DataItem> dataItemTreeItem = mainController.getTreeViewDataSource().getSelectionModel().getSelectedItem();

        // 从根节点删除数据源
        mainController.getTreeItemDataSourceRoot().getChildren().remove(dataItemTreeItem);

        DataSource dataSource = (DataSource) dataItemTreeItem.getValue();

        // 关闭右边的 Border 展示
        this.rightBorderShowClose(dataSource);
        
        // 删除对应的文件
        dataSourceService.deleteDataSource(dataSource);
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
     * 关闭右边的 Border 展示
     *
     * @param dataSource 数据源
     */
    private void rightBorderShowClose(DataSource dataSource) {
        if (BaseConstants.selectedDateSource == null || BaseConstants.selectedDateSource == dataSource) {
            final BorderPane borderPane1 = mainController.getBorderPane1();
            borderPane1.setVisible(false);
            borderPane1.setManaged(false);
        }
    }
}
