package com.alan344.init;

import com.alan344.bean.Column;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConditionEnum;
import com.alan344.convert.ConditionEnumConvert;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyDoubleProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.ComboBoxTableCell;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.springframework.stereotype.Service;

import java.util.Collection;

/**
 * @author AlanSun
 * @date 2020/9/9 10:17
 * <p>
 * 业务逻辑配置初始化
 */
@Service
public class ServiceListViewInit {

    /*------------------------------------ListView ContextMenu--------------------------------------------------------*/

    /**
     * 设置listView
     *
     * @param tables 已选表
     */
    public ObservableList<VBox> setListView(Collection<Table> tables, ListView<VBox> vBoxListView) {
        if (tables == null) {
            vBoxListView.setItems(null);
            return FXCollections.emptyObservableList();
        }

        ObservableList<VBox> vBoxes = FXCollections.observableArrayList();
        vBoxListView.setItems(vBoxes);

        for (Table table : tables) {
            Label tableNameLabel = new Label(table.getTableName());
            tableNameLabel.setStyle("-fx-font-size: 18; -fx-font-weight: bold;");

            Button expand = new Button();
            expand.setGraphic(new ImageView("/image/expand.png"));
            expand.setStyle("-fx-background-color: transparent");
            expand.setPrefWidth(80);
            expand.setOnAction(event -> {
                Button source = (Button) event.getSource();
                VBox selectedVBox = ((VBox) source.getParent().getParent());

                ObservableList<Node> children = selectedVBox.getChildren();
                if (children.size() == 1) {
                    // 关闭
                    expand.setGraphic(new ImageView("/image/close.png"));
                    this.expandTableViewColumns(selectedVBox);
                } else {
                    // 展开
                    HBox tableView = (HBox) children.get(1);
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

            HBox hBox2 = new HBox(20, tableNameLabel, expand);
            hBox2.setAlignment(Pos.CENTER);

            VBox container = new VBox(10, hBox2);
            vBoxes.add(container);
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

        String tableName = ((Label) ((HBox) selectedVBox.getChildren().get(0)).getChildren().get(0)).getText();
        TableView<Column> columnTableView = new TableView<>(FXCollections.observableArrayList(BaseConstants.selectedTableNameTableMap.get(tableName).getColumns()));
        columnTableView.setEditable(true);
        columnTableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

        ReadOnlyDoubleProperty widthBind = hBox.widthProperty();

        // 字段名
        TableColumn<Column, String> tcColumnNam = new TableColumn<>("字段名");
        tcColumnNam.setCellValueFactory(param -> new SimpleStringProperty(param.getValue().getColumnName()));
        tcColumnNam.setSortable(false);
        tcColumnNam.prefWidthProperty().bind(widthBind.multiply(0.5));
        tcColumnNam.getStyleClass().setAll(columnStyleClass);

        // 条件
        TableColumn<Column, ConditionEnum> conditionColumn = new TableColumn<>("条件");
        conditionColumn.setCellFactory(param -> {
            ComboBoxTableCell<Column, ConditionEnum> cell = new ComboBoxTableCell<>(ConditionEnum.getAll());
            cell.setConverter(new ConditionEnumConvert());
            return cell;
        });
        conditionColumn.setOnEditCommit(event -> {
            event.getRowValue().setCondition(event.getNewValue().getSymbol());
        });
        conditionColumn.setSortable(false);
        conditionColumn.prefWidthProperty().bind(widthBind.multiply(0.5));
        conditionColumn.getStyleClass().setAll(columnStyleClass);

        columnTableView.getColumns().add(tcColumnNam);
        columnTableView.getColumns().add(conditionColumn);

        columnTableView.setFixedCellSize(30);
        columnTableView.prefHeightProperty().bind(columnTableView.fixedCellSizeProperty().multiply(Bindings.size(columnTableView.getItems()).add(1.3)));
        columnTableView.prefWidthProperty().bind(hBox.widthProperty());

        hBox.getChildren().add(columnTableView);
        selectedVBox.getChildren().add(hBox);
    }
}
