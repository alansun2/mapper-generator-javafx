package com.alan344.init;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.controller.MainController;
import com.alan344.service.ColumnService;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Toast;
import com.alan344.utils.TreeUtils;
import javafx.collections.ObservableList;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
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

    /**
     * treeView init
     */
    public void treeViewInit(TreeView<DataItem> treeViewDataSource, BorderPane borderPane1, BorderPane borderPane, TreeItem<DataItem> treeItemDataSourceRoot, ListView<VBox> vBoxListView) {
        // 设置鼠标释放事件
        treeViewDataSource.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
            if (event.getButton() == MouseButton.SECONDARY) {
                // 右键释放

                //item is selected - this prevents fail when clicking on empty space
                ContextMenu contextMenu;
                if (selectedItems != null) {
                    //open context menu on current screen position
                    MenuItem exportMenuItem = new MenuItem("导出");
                    exportMenuItem.setGraphic(new ImageView("/image/export-datasource@16.png"));
                    exportMenuItem.setOnAction(event1 -> export(treeViewDataSource, borderPane1, vBoxListView));
                    if (selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {

                        MenuItem refreshMenuItem = new MenuItem("刷新");
                        refreshMenuItem.setGraphic(new ImageView("/image/refresh@16.png"));
                        refreshMenuItem.setOnAction(event1 -> refreshDataSource(treeViewDataSource, borderPane1));
                        MenuItem deleteMenuItem = new MenuItem("删除数据源");
                        deleteMenuItem.setGraphic(new ImageView("/image/delete@16.png"));
                        deleteMenuItem.setOnAction(event1 -> deleteDataSource(treeItemDataSourceRoot, treeViewDataSource, borderPane1, borderPane));
                        contextMenu = new ContextMenu(exportMenuItem, refreshMenuItem, deleteMenuItem);
                    } else {
                        contextMenu = new ContextMenu(exportMenuItem);
                    }
                    treeViewDataSource.setContextMenu(contextMenu);
                }
            } else if (event.getButton() == MouseButton.PRIMARY && selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {
                // 左键释放时，切换 listView
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
    private void export(TreeView<DataItem> treeViewDataSource, BorderPane borderPane1, ListView<VBox> vBoxListView) {
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
            for (TreeItem<DataItem> selectedItem : selectedItems) {
                DataItem dataItem = selectedItem.getValue();
                if (dataItem instanceof Table) {
                    Table table = (Table) dataItem;
                    tables.add(table);
                }
            }

            BaseConstants.selectedDateSource = ((DataSource) selectedItems.get(0).getParent().getValue());
        }
        // 清空当前checkBoxVBox
        BaseConstants.selectedCheckBoxVBox.clear();

        // 把选中要导出的表在右边的listView展示
        rightListViewInit.setListView(tables, vBoxListView);
        // 选中的表放入map
        BaseConstants.selectedTableNameTableMap = tables.stream().collect(Collectors.toMap(Table::getTableName, o -> o));
        // 用于当再不同的 dataSource 之间切换时，保留原来的 tables
        BaseConstants.dataSourceTableListMap.put(BaseConstants.selectedDateSource, tables);

        // 如果没有字段，则从远程加载
        tables.forEach(table -> columnService.reloadColumnsIfNotNull(table));

        //show rightBorderTopHBox
        if (!borderPane1.isVisible() && !borderPane1.isManaged()) {
            borderPane1.setVisible(true);
            borderPane1.setManaged(true);
        }
    }

    /**
     * 删除数据源
     */
    private void deleteDataSource(TreeItem<DataItem> treeItemDataSourceRoot, TreeView<DataItem> treeViewDataSource, BorderPane borderPane1, BorderPane borderPane) {
        Stage primaryStage = (Stage) borderPane.getScene().getWindow();
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        if (selectedItems.size() != 1) {
            Toast.makeText(primaryStage, "只能选择一个", 3000, 500, 500, 15, 5);
            return;
        }

        TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
        if (dataItemTreeItem.getValue() instanceof Table) {
            Toast.makeText(primaryStage, "无法对表进行操作", 3000, 500, 500, 15, 5);
            return;
        }

        treeItemDataSourceRoot.getChildren().remove(dataItemTreeItem);

        DataSource dataSource = (DataSource) dataItemTreeItem.getValue();
        dataSourceService.deleteDataSource(dataSource);

        if (BaseConstants.selectedDateSource == null || BaseConstants.selectedDateSource == dataSource) {
            borderPane1.setVisible(false);
            borderPane1.setManaged(false);
        }
    }


    /**
     * 刷新数据源下的 table
     * <p>
     * 对table
     */
    private void refreshDataSource(TreeView<DataItem> treeViewDataSource, BorderPane borderPane1) {
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

        if (BaseConstants.selectedDateSource == null || BaseConstants.selectedDateSource == dataSource) {
            borderPane1.setVisible(false);
            borderPane1.setManaged(false);
        }
    }
}
