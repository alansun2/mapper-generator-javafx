package com.alan344.init;

import com.alan344.bean.Column;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import lombok.extern.slf4j.Slf4j;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author AlanSun
 * @date 2020/4/2 21:56
 */
@Slf4j
@Service
public class MybatisListViewInit {

    /**
     * 在同一个 dataSource 中用左键点击时判断是否是上一次的 dataSource。如果是的话就不重新加载 listView
     */
    private DataSource lastDataSource;

    /**
     * 设置右键监听
     */
    public void addListener(ListView<VBox> vBoxListView) {
        ContextMenu contextMenu = vBoxListView.getContextMenu();
        vBoxListView.addEventHandler(MouseEvent.MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                ObservableList<VBox> selectedItems = vBoxListView.getSelectionModel().getSelectedItems();
                if (selectedItems.size() != 1) {
                    vBoxListView.setContextMenu(null);
                } else {
                    vBoxListView.setContextMenu(contextMenu);
                }
            }
        });
    }

    /**
     * 切换 ListView
     *
     * @param dataSource 数据源
     */
    public void treeViewSwitch(DataSource dataSource) {
        if (lastDataSource == null || lastDataSource != dataSource) {
            lastDataSource = dataSource;
        } else {
            return;
        }

        // 选中的 DataSource
        BaseConstants.selectedDateSource = dataSource;

        ObservableList<VBox> vBoxes = BaseConstants.dataSourceTableVBoxListMap.get(dataSource);
        // 选中的表放入 map
        BaseConstants.selectedTableNameTableMap = BaseConstants.dataSourceTableListMap.get(dataSource);

        if (vBoxes != null) {
            NodeConstants.mybatisListView.setItems(vBoxes);
        } else {
            this.setListView(null);
        }
    }

    /*------------------------------------ListView ContextMenu--------------------------------------------------------*/

    /**
     * 设置listView
     *
     * @param tables 已选表
     */
    public ObservableList<VBox> setListView(List<Table> tables) {
        ListView<VBox> vBoxListView = NodeConstants.mybatisListView;
        if (tables == null) {
            vBoxListView.setItems(null);
            return FXCollections.emptyObservableList();
        }

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
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox insert = new CheckBox("insert");
            insert.setSelected(table.isInsert());
            insert.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isInsert() != newValue) {
                    table.setInsert(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox count = new CheckBox("count");
            count.setSelected(table.isCount());
            count.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isCount() != newValue) {
                    table.setCount(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox update = new CheckBox("update");
            update.setSelected(table.isSelect());
            update.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isUpdate() != newValue) {
                    table.setUpdate(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox delete = new CheckBox("delete");
            delete.setSelected(table.isDelete());
            delete.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isDelete() != newValue) {
                    table.setDelete(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox select = new CheckBox("select");
            select.setSelected(table.isSelect());
            select.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isSelect() != newValue) {
                    table.setSelect(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
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
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox deleteExample = new CheckBox("deleteExample");
            deleteExample.setSelected(table.isDeleteExample());
            deleteExample.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isDeleteExample() != newValue) {
                    table.setDeleteExample(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            CheckBox selectExample = new CheckBox("selectExample");
            selectExample.setSelected(table.isSelectExample());
            selectExample.selectedProperty().addListener((observable, oldValue, newValue) -> {
                if (table.isSelectExample() != newValue) {
                    table.setSelectExample(newValue);
                    BaseConstants.tableNameSetUpTableRecordMap.put(tableName, true);
                }
            });

            HBox checkBoxHBox2 = new HBox(updateExample, deleteExample, selectExample);
            checkBoxHBox2.setAlignment(Pos.CENTER);
            checkBoxHBox2.setSpacing(15);

            VBox checkBoxVBox = new VBox(checkBoxHBox2, checkBoxHBox1);
            checkBoxVBox.setSpacing(15);

            BaseConstants.selectedCheckBoxVBox.add(checkBoxVBox);

            // 展开按钮
            Button expand = new Button();
            expand.setGraphic(new FontIcon("unim-angle-down:16:GRAY"));
            expand.setStyle("-fx-background-color: transparent");
            expand.setPrefWidth(80);
            expand.setOnAction(event -> {
                Button source = (Button) event.getSource();
                VBox selectedVBox = ((VBox) source.getParent().getParent());

                ObservableList<Node> children = selectedVBox.getChildren();
                if (children.size() == 2) {
                    // 展开状态
                    expand.setGraphic(new FontIcon("unim-angle-up:16:GRAY"));
                    this.expandTableViewColumns(selectedVBox);
                } else {
                    // 展开状态时，tableView 就是第三个
                    HBox tableView = (HBox) children.get(2);
                    if (tableView.isVisible()) {
                        expand.setGraphic(new FontIcon("unim-angle-down:16:GRAY"));
                        tableView.setVisible(false);
                        tableView.setManaged(false);
                    } else {
                        expand.setGraphic(new FontIcon("unim-angle-up:16:GRAY"));
                        tableView.setVisible(true);
                        tableView.setManaged(true);
                    }
                }
            });

            HBox hBox2 = new HBox(20, checkBoxVBox, expand);
            hBox2.setAlignment(Pos.CENTER);

            vBoxes.add(new VBox(10, tableNameLabelHBox, hBox2));
        }

        return vBoxes;
    }

    //-----------------------------------------tableView--------------------------------------------------------------//

    /**
     * 展开字段
     */
    public void expandTableViewColumns(VBox selectedVBox) {
        HBox hBox = new HBox();
        hBox.setAlignment(Pos.CENTER);

        String columnStyleClass = "myColumn";

        String tableName = ((Label) (((HBox) selectedVBox.getChildren().get(0))).getChildren().get(0)).getText();
        TableView<Column> columnTableView = new TableView<>(FXCollections.observableArrayList(BaseConstants.selectedTableNameTableMap.get(tableName).getColumns()));
        columnTableView.setEditable(true);
        columnTableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

        ReadOnlyDoubleProperty widthBind = hBox.widthProperty();

        TableColumn<Column, String> tcColumnNam = new TableColumn<>("字段名");
        tcColumnNam.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnName()));
        tcColumnNam.setSortable(false);
        tcColumnNam.prefWidthProperty().bind(widthBind.multiply(0.16));
        tcColumnNam.getStyleClass().setAll("columnStyleClass");

        TableColumn<Column, String> tcType = new TableColumn<>("类型");
        tcType.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getType()));
        tcType.setSortable(false);
        tcType.prefWidthProperty().bind(widthBind.multiply(0.16));
        tcType.getStyleClass().setAll(columnStyleClass);

        TableColumn<Column, String> property = new TableColumn<>("property");
        property.setCellFactory(TextFieldTableCell.forTableColumn());
        property.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnOverride().getProperty()));
        property.setOnEditCommit(event -> {
            event.getRowValue().getColumnOverride().setProperty(event.getNewValue());
            BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
        });
        property.setSortable(false);
        property.prefWidthProperty().bind(widthBind.multiply(0.16));
        property.getStyleClass().setAll(columnStyleClass);

        TableColumn<Column, String> javaType = new TableColumn<>("java type");
        javaType.setCellFactory(TextFieldTableCell.forTableColumn());
        javaType.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnOverride().getJavaType()));
        javaType.setOnEditCommit(event -> {
            event.getRowValue().getColumnOverride().setJavaType(event.getNewValue());
            BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
        });
        javaType.setSortable(false);
        javaType.prefWidthProperty().bind(widthBind.multiply(0.2));
        javaType.getStyleClass().setAll(columnStyleClass);

        TableColumn<Column, String> typeHandler = new TableColumn<>("type handler");
        typeHandler.setCellFactory(TextFieldTableCell.forTableColumn());
        typeHandler.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnOverride().getTypeHandler()));
        typeHandler.setOnEditCommit(event -> {
            event.getRowValue().getColumnOverride().setTypeHandler(event.getNewValue());
            BaseConstants.tableNameIsOverrideRecodeMap.put(tableName, true);
        });
        typeHandler.setSortable(false);
        typeHandler.prefWidthProperty().bind(widthBind.multiply(0.22));
        typeHandler.getStyleClass().setAll(columnStyleClass);

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
        ignoreCheckBox.getStyleClass().setAll(columnStyleClass);

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
}
