package com.alan344.controller;

import com.alan344.bean.Column;
import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.service.ColumnService;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Toast;
import com.alan344.utils.TreeUtils;
import javafx.application.HostServices;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.image.ImageView;
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
import java.util.stream.Collectors;

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
    private ListView<VBox> vBoxListView;

    @FXML
    private TreeView<DataItem> treeViewDataSource;

    @FXML
    private CheckBox insertReturnCheckBox;
    @FXML
    private CheckBox insertCheckBox;
    @FXML
    private CheckBox countCheckBox;
    @FXML
    private CheckBox updateCheckBox;
    @FXML
    private CheckBox updateExampleCheckBox;
    @FXML
    private CheckBox deleteCheckBox;
    @FXML
    private CheckBox deleteExampleCheckBox;
    @FXML
    private CheckBox selectCheckBox;
    @FXML
    private CheckBox selectExampleCheckBox;

    @Autowired
    private DateSourceController dateSourceController;

    @Autowired
    private ConfigController configController;

    @Autowired
    private AboutController aboutController;

    @Autowired
    private DataSourceService dataSourceService;

    @Autowired
    private TableService tableService;

    @Autowired
    private ColumnService columnService;

    @Autowired
    private BeanFactory beanFactory;

    private List<VBox> selectedCheckBoxVBox = new ArrayList<>();

    //--------------------------------init----------------------------------------------------------------------------//

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        menuBar.prefWidthProperty().bind(borderPane.widthProperty());

        this.treeViewInit();

        this.checkBoxInit();

        //从文件加载数据源至pane
        this.loadData();
    }

    /**
     * 加载数据文件
     */
    private void loadData() {
        //从文件加载数据源至pane
        List<DataSource> dataSources = dataSourceService.loadDataSourceFromFile();
        if (!dataSources.isEmpty()) {
            for (DataSource dataSource : dataSources) {
                TreeItem<DataItem> dataSourceTreeItem = TreeUtils.add2Tree(dataSource, treeItemRoot);
                dataSourceTreeItem.setGraphic(new ImageView("/image/database.png"));
                List<Table> tables = dataSource.getTables();
                if (tables != null && !tables.isEmpty()) {
                    tables.forEach(table -> {
                        TreeItem<DataItem> tableTreeItem = TreeUtils.add2Tree(table, dataSourceTreeItem);
                        tableTreeItem.setGraphic(new ImageView("/image/table.png"));
                    });
                }
            }
        }
        if (dataSources.size() == 1) {
            treeItemRoot.getChildren().get(0).setExpanded(true);
        }
    }

    /**
     * treeView init
     */
    private void treeViewInit() {
        //设置多选
        treeViewDataSource.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

        treeViewDataSource.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();

                //item is selected - this prevents fail when clicking on empty space
                ContextMenu contextMenu;
                if (selectedItems != null) {
                    //open context menu on current screen position
                    MenuItem exportMenuItem = new MenuItem("导出");
                    exportMenuItem.setGraphic(new ImageView("/image/export-datasource@16.png"));
                    exportMenuItem.setOnAction(event1 -> export());
                    if (selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {

                        MenuItem refreshMenuItem = new MenuItem("刷新");
                        refreshMenuItem.setGraphic(new ImageView("/image/refresh@16.png"));
                        refreshMenuItem.setOnAction(event1 -> refreshDataSource());
                        MenuItem deleteMenuItem = new MenuItem("删除数据源");
                        deleteMenuItem.setGraphic(new ImageView("/image/delete@16.png"));
                        deleteMenuItem.setOnAction(event1 -> deleteDataSource());
                        contextMenu = new ContextMenu(exportMenuItem, refreshMenuItem, deleteMenuItem);
                    } else {
                        contextMenu = new ContextMenu(exportMenuItem);
                    }
                    treeViewDataSource.setContextMenu(contextMenu);
                }
            }
        });
    }

    /**
     * checkbox init
     */
    private void checkBoxInit() {
        insertReturnCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, 0, newValue));
        insertCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, 1, newValue));
        countCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, 2, newValue));
        updateCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, 3, newValue));
        deleteCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, 4, newValue));
        selectCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, 5, newValue));
        updateExampleCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(0, 0, newValue));
        deleteExampleCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(0, 1, newValue));
        selectExampleCheckBox.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(0, 2, newValue));
    }

    private void checkBoxAction(int columnIndex, int rowIndex, boolean selected) {
        if (!selectedCheckBoxVBox.isEmpty()) {
            selectedCheckBoxVBox.forEach(vBox -> ((CheckBox) ((HBox) vBox.getChildren().get(columnIndex)).getChildren().get(rowIndex)).setSelected(selected));
        }
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
        //清空当前checkBoxVBox
        selectedCheckBoxVBox.clear();

        //把选中要导出的表在右边的listView展示
        this.setListView(tables);
        //选中的表
        BaseConstants.selectedTableNameTableMap = tables.stream().collect(Collectors.toMap(Table::getTableName, o -> o));

        //如果没有字段，则从远程加载
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
    private void deleteDataSource() {
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

        treeItemRoot.getChildren().remove(dataItemTreeItem);

        DataSource dataSource = (DataSource) dataItemTreeItem.getValue();
        dataSourceService.deleteDataSource(dataSource);

        if (BaseConstants.selectedDateSource == null || BaseConstants.selectedDateSource == dataSource) {
            borderPane1.setVisible(false);
            borderPane1.setManaged(false);
        }
    }

    /**
     * 刷新数据源下的table
     * <p>
     * 对table
     */
    private void refreshDataSource() {
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

    /*------------------------------------ListView ContextMenu--------------------------------------------------------*/

    /**
     * 设置listView
     *
     * @param tables 已选表
     */
    private void setListView(List<Table> tables) {
        ObservableList<VBox> vBoxes = FXCollections.observableArrayList();
        vBoxListView.setItems(vBoxes);

        for (Table table : tables) {
            String tableName = table.getTableName();
            Label tableNameLabel = new Label(tableName);
            tableNameLabel.setStyle("-fx-font-size: 18; -fx-font-weight: bold;");
            HBox tableNameLabelHBox = new HBox(tableNameLabel);
            tableNameLabelHBox.setAlignment(Pos.CENTER);

            CheckBox returnId = new CheckBox("insert返回id");
            returnId.setSelected(table.isReturnInsertId());
            returnId.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isReturnInsertId() != newValue) {
                    table.setReturnInsertId(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox insert = new CheckBox("insert");
            insert.setSelected(table.isInsert());
            insert.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isInsert() != newValue) {
                    table.setInsert(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox count = new CheckBox("count");
            count.setSelected(table.isCount());
            count.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isCount() != newValue) {
                    table.setCount(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox update = new CheckBox("update");
            update.setSelected(table.isSelect());
            update.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isUpdate() != newValue) {
                    table.setUpdate(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox delete = new CheckBox("delete");
            delete.setSelected(table.isDelete());
            delete.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isDelete() != newValue) {
                    table.setDelete(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox select = new CheckBox("select");
            select.setSelected(table.isSelect());
            select.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isSelect() != newValue) {
                    table.setSelect(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            HBox checkBoxHBox1 = new HBox(returnId, insert, count, update, delete, select);
            checkBoxHBox1.setAlignment(Pos.CENTER);
            checkBoxHBox1.setSpacing(15);

            CheckBox updateExample = new CheckBox("updateExample");
            updateExample.setSelected(table.isUpdateExample());
            updateExample.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isUpdateExample() != newValue) {
                    table.setUpdateExample(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox deleteExample = new CheckBox("deleteExample");
            deleteExample.setSelected(table.isDeleteExample());
            deleteExample.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isSelectExample() != newValue) {
                    table.setDeleteExample(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            CheckBox selectExample = new CheckBox("selectExample");
            selectExample.setSelected(table.isSelectExample());
            selectExample.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isSelectExample() != newValue) {
                    table.setSelectExample(newValue);
                    BaseConstants.tableNameIsTableRecordMap.put(tableName, true);
                }
            });

            HBox checkBoxHBox2 = new HBox(updateExample, deleteExample, selectExample);
            checkBoxHBox2.setAlignment(Pos.CENTER);
            checkBoxHBox2.setSpacing(15);

            VBox checkBoxVBox = new VBox(checkBoxHBox2, checkBoxHBox1);
            checkBoxVBox.setSpacing(15);

            selectedCheckBoxVBox.add(checkBoxVBox);

            Button expand = new Button();
            expand.setGraphic(new ImageView("/image/expand.png"));
            expand.setStyle("-fx-background-color: transparent");
            expand.setPrefWidth(80);
            expand.setOnAction(event -> {
                Button source = (Button) event.getSource();
                VBox selectedVBox = ((VBox) source.getParent().getParent());

                ObservableList<Node> children = selectedVBox.getChildren();
                if (children.size() == 2) {
                    expand.setGraphic(new ImageView("/image/close.png"));
                    this.expandTableViewColumns(selectedVBox);
                } else {
                    HBox tableView = (HBox) children.get(2);
                    if (tableView.isVisible()) {
                        expand.setGraphic(new ImageView("/image/expand.png"));
                        tableView.setVisible(false);
                        tableView.setManaged(false);
                    } else {
                        expand.setGraphic(new ImageView("/image/close.png"));
                        tableView.setVisible(true);
                        tableView.setManaged(true);
                    }
                }
            });

            HBox hBox2 = new HBox(20, checkBoxVBox, expand);
            hBox2.setAlignment(Pos.CENTER);

            VBox vBox = new VBox(10, tableNameLabelHBox, hBox2);
            vBoxes.add(vBox);
        }
    }

    /**
     * 刷新字段信息
     */
    @FXML
    private void refreshTableColumn() {
        VBox selectedItemVBox = vBoxListView.getSelectionModel().getSelectedItem();
        String tableName = ((Label) ((HBox) selectedItemVBox.getChildren().get(0)).getChildren().get(0)).getText();
        columnService.reloadColumns(tableName);
        selectedItemVBox.getChildren().remove(2);
        this.expandTableViewColumns(selectedItemVBox);
    }

    //-----------------------------------------tableView--------------------------------------------------------------//

    /**
     * 展开字段
     */
    private void expandTableViewColumns(VBox selectedVBox) {
        HBox hBox = new HBox();
        hBox.setAlignment(Pos.CENTER);

        String tableName = ((Label) (((HBox) selectedVBox.getChildren().get(0))).getChildren().get(0)).getText();
        TableView<Column> columnTableView = new TableView<>(FXCollections.observableArrayList(BaseConstants.selectedTableNameTableMap.get(tableName).getColumns()));
        columnTableView.setEditable(true);
        columnTableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

        ReadOnlyDoubleProperty widthBind = hBox.widthProperty();

        TableColumn<Column, String> tcColumnNam = new TableColumn<>("字段名");
        tcColumnNam.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnName()));
        tcColumnNam.setSortable(false);
        tcColumnNam.prefWidthProperty().bind(widthBind.multiply(0.16));
        tcColumnNam.getStyleClass().setAll("myColumn");

        TableColumn<Column, String> tcType = new TableColumn<>("类型");
        tcType.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getType()));
        tcType.setSortable(false);
        tcType.prefWidthProperty().bind(widthBind.multiply(0.16));
        tcType.getStyleClass().setAll("myColumn");

        TableColumn<Column, String> property = new TableColumn<>("property");
        property.setCellFactory(TextFieldTableCell.forTableColumn());
        property.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnOverride().getProperty()));
        property.setOnEditCommit(event -> {
            event.getRowValue().getColumnOverride().setProperty(event.getNewValue());
            BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
        });
        property.setSortable(false);
        property.prefWidthProperty().bind(widthBind.multiply(0.16));
        property.getStyleClass().setAll("myColumn");

        TableColumn<Column, String> javaType = new TableColumn<>("java type");
        javaType.setCellFactory(TextFieldTableCell.forTableColumn());
        javaType.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnOverride().getJavaType()));
        javaType.setOnEditCommit(event -> {
            event.getRowValue().getColumnOverride().setJavaType(event.getNewValue());
            BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
        });
        javaType.setSortable(false);
        javaType.prefWidthProperty().bind(widthBind.multiply(0.2));
        javaType.getStyleClass().setAll("myColumn");

        TableColumn<Column, String> typeHandler = new TableColumn<>("type handler");
        typeHandler.setCellFactory(TextFieldTableCell.forTableColumn());
        typeHandler.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnOverride().getTypeHandler()));
        typeHandler.setOnEditCommit(event -> {
            event.getRowValue().getColumnOverride().setTypeHandler(event.getNewValue());
            BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
        });
        typeHandler.setSortable(false);
        typeHandler.prefWidthProperty().bind(widthBind.multiply(0.22));
        typeHandler.getStyleClass().setAll("myColumn");

        TableColumn<Column, Boolean> ignoreCheckBox = new TableColumn<>("是否忽略");
        ignoreCheckBox.setCellFactory(CheckBoxTableCell.forTableColumn(param -> {
            final Column column = columnTableView.getItems().get(param);
            column.ignoreProperty().addListener((observable, oldValue, newValue) -> {
                column.setIgnore(newValue);
                BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
            });
            return column.ignoreProperty();
        }));
        ignoreCheckBox.setSortable(false);
        ignoreCheckBox.prefWidthProperty().bind(widthBind.multiply(0.1));
        ignoreCheckBox.getStyleClass().setAll("myColumn");

        columnTableView.getColumns().add(tcColumnNam);
        columnTableView.getColumns().add(tcType);
        columnTableView.getColumns().add(ignoreCheckBox);
        columnTableView.getColumns().add(property);
        columnTableView.getColumns().add(javaType);
        columnTableView.getColumns().add(typeHandler);

        columnTableView.setFixedCellSize(30);
        columnTableView.prefHeightProperty().bind(columnTableView.fixedCellSizeProperty().multiply(Bindings.size(columnTableView.getItems()).add(1.3)));
        columnTableView.prefWidthProperty().bind(hBox.widthProperty());

        hBox.getChildren().add(columnTableView);
        selectedVBox.getChildren().add(hBox);
    }

    //-------------

    /**
     * 导出窗口
     *
     * @throws IOException e
     */
    @FXML
    public void openConfigWindow() throws IOException {
        configController.openConfigPane((Stage) borderPane.getScene().getWindow());
    }

    @FXML
    public void openAboutWindow() throws IOException {
        aboutController.openWindow((Stage) borderPane.getScene().getWindow());
    }
}
