package com.alan344.controller;

import com.alan344.bean.Column;
import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Toast;
import javafx.application.HostServices;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/7 17:04
 */
@Controller
public class MainController implements Initializable {
    @FXML
    private BorderPane borderPane;

    @FXML
    private BorderPane borderPane1;

    @FXML
    private MenuBar menuBar;

    @Getter
    @FXML
    private TreeItem<DataItem> treeItemRoot;

    @Getter
    @FXML
    private ListView<VBox> anchorPaneListView;

    @FXML
    private TreeView<DataItem> treeViewDataSource;

    @Autowired
    private DateSourceController dateSourceController;

    @Autowired
    private ExportController exportController;

    @Autowired
    private DataSourceService dataSourceService;

    @Autowired
    private TableService tableService;

    @Autowired
    private BeanFactory beanFactory;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        menuBar.prefWidthProperty().bind(borderPane.widthProperty());

        treeViewDataSource.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

        treeViewDataSource.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();

                //item is selected - this prevents fail when clicking on empty space
                ContextMenu contextMenu;
                if (selectedItems != null) {
                    //open context menu on current screen position
                    MenuItem menuItem = new MenuItem("导出");
                    menuItem.setOnAction(event1 -> export());
                    if (selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {

                        MenuItem menuItem2 = new MenuItem("修改数据源");
                        MenuItem menuItem3 = new MenuItem("删除数据源");
                        menuItem3.setOnAction(event1 -> deleteDataSource());
                        contextMenu = new ContextMenu(menuItem, menuItem2, menuItem3);
                    } else {
                        contextMenu = new ContextMenu(menuItem);
                    }
                    treeViewDataSource.setContextMenu(contextMenu);
                }
            }
        });

        //listView
        anchorPaneListView.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                ObservableList<Node> children = anchorPaneListView.getSelectionModel().getSelectedItem().getChildren();
                MenuItem menuItem1 = new MenuItem("刷新");
                menuItem1.setOnAction(event1 -> refresh());
                if (children.size() == 2) {
                    MenuItem menuItem2 = new MenuItem("展开");
                    menuItem2.setOnAction(event1 -> expand());
                    anchorPaneListView.setContextMenu(new ContextMenu(menuItem1, menuItem2));
                } else {
                    TableView tableView = (TableView) children.get(2);
                    if (tableView.isVisible()) {
                        MenuItem menuItem2 = new MenuItem("闭合");
                        menuItem2.setOnAction(event1 -> {
                            tableView.setVisible(false);
                            tableView.setManaged(false);
                        });
                        anchorPaneListView.setContextMenu(new ContextMenu(menuItem1, menuItem2));
                    } else {
                        MenuItem menuItem2 = new MenuItem("展开");
                        menuItem2.setOnAction(event1 -> {
                            tableView.setVisible(true);
                            tableView.setManaged(true);
                        });
                        anchorPaneListView.setContextMenu(new ContextMenu(menuItem1, menuItem2));
                    }
                }
            }
        });

        //从文件加载数据源至pane
        dataSourceService.loadDataSourceFromFile(treeItemRoot);
    }

    /*---------------------------------------MenuBar------------------------------------------------------------------*/

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() throws IOException {
        dateSourceController.addDataSource((Stage) borderPane.getScene().getWindow());
    }

    @FXML
    public void exit() {
        Platform.exit();
    }

    @FXML
    public void generateConfig() throws IOException {
        exportController.export((Stage) borderPane.getScene().getWindow());
    }

    @FXML
    public void openGithub() {
        HostServices hostServices = beanFactory.getBean(HostServices.class);
        hostServices.showDocument("https://github.com/alansun2/mapper-generator-javafx");
    }

    /*------------------------------------TreeView ContextMenu--------------------------------------------------------*/

    /**
     * 全部导出
     */
    private void export() {
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        List<Table> tables;
        if (selectedItems.size() == 1) {
            TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
            if (dataItemTreeItem.getValue() instanceof DataSource) {
                //TODO 生成xml文件
                ObservableList<TreeItem<DataItem>> children = dataItemTreeItem.getChildren();
                tables = new ArrayList<>();
                if (!children.isEmpty()) {
                    children.forEach(itemTreeItem -> tables.add(((Table) itemTreeItem.getValue())));
                }
            } else {
                //TODO 生成xml文件
                Table table = (Table) dataItemTreeItem.getValue();
                tables = Collections.singletonList(table);
            }
        } else {
            //TODO 生成xml文件
            tables = new ArrayList<>();
            for (TreeItem<DataItem> selectedItem : selectedItems) {
                DataItem dataItem = selectedItem.getValue();
                if (dataItem instanceof Table) {
                    Table table = (Table) dataItem;
                    tables.add(table);
                }
            }
        }
        tableService.setListView(tables, anchorPaneListView);
    }

    /**
     * 删除数据源
     */
    private void deleteDataSource() {
        Stage primaryStage = (Stage) borderPane.getScene().getWindow();
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        if (selectedItems.size() != 1) {
            Toast.Companion.makeText(primaryStage, "只能选择一个", 3000, 500, 500, 15, 5);
            return;
        }

        TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
        if (dataItemTreeItem.getValue() instanceof Table) {
            Toast.Companion.makeText(primaryStage, "无法对表进行操作", 3000, 500, 500, 15, 5);
            return;
        }

        treeItemRoot.getChildren().remove(dataItemTreeItem);

        dataSourceService.deleteDataSource(((DataSource) dataItemTreeItem.getValue()));
    }

    /*------------------------------------ListView ContextMenu--------------------------------------------------------*/

    private void refresh() {

    }

    /**
     * 展开字段
     */
    private void expand() {
        VBox selectedItem = anchorPaneListView.getSelectionModel().getSelectedItem();

        String tableName = ((Label) (((HBox) selectedItem.getChildren().get(0))).getChildren().get(0)).getText();
        List<Column> columns = tableService.getColumns(tableName);

        ObservableList<Column> gridPanes = FXCollections.observableArrayList(columns);
        TableView<Column> columnTableView = new TableView<>(gridPanes);

        double borderPane1Width = borderPane1.getWidth();
        double columnWidth = borderPane1Width / 2;

        TableColumn<Column, String> tcColumnNam = new TableColumn<>("字段名");
        tcColumnNam.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnName()));
        tcColumnNam.setPrefWidth(columnWidth);

        TableColumn<Column, String> tcType = new TableColumn<>("类型");
        tcType.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getType()));
        tcType.setPrefWidth(columnWidth);

        columnTableView.getColumns().add(tcColumnNam);
        columnTableView.getColumns().add(tcType);

        columnTableView.setFixedCellSize(25);
        columnTableView.prefHeightProperty().bind(columnTableView.fixedCellSizeProperty().multiply(Bindings.size(columnTableView.getItems()).add(1.01)));
        selectedItem.getChildren().add(columnTableView);

        MenuItem overrideColumnMenuItem = new MenuItem("重写");
        overrideColumnMenuItem.setOnAction(event -> {

        });
        columnTableView.setContextMenu(new ContextMenu(overrideColumnMenuItem));
    }

    //-------------

    /**
     * 导出窗口
     *
     * @throws IOException e
     */
    public void exportXml() throws IOException {
        exportController.export((Stage) borderPane.getScene().getWindow());
    }
}
