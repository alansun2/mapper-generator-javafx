package com.alan344.controller;

import com.alan344.bean.Column;
import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.controller.component.TableAdvanceSetupController;
import com.alan344.init.MapperCheckBoxInit;
import com.alan344.init.MybatisListViewInit;
import com.alan344.service.ColumnService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.Assert;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Alert;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TableView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @since 2020/4/7 17:09
 */
@Controller
@Slf4j
public class MybatisTableSetupController implements Initializable {
    @FXML
    private BorderPane borderPane;
    @FXML
    private StackPane mainStackPane;
    @FXML
    private ListView<VBox> listView;
    /**
     * 右边 border 固定再上面的 两个 HBox。存放 checkBox
     */
    @FXML
    private HBox mapperCheckBoxHbox1;
    @FXML
    private HBox mapperCheckBoxHbox2;
    @Autowired
    private TableAdvanceSetupController tableAdvanceSetUpController;
    @Autowired
    private ColumnService columnService;
    @Autowired
    private MybatisListViewInit mybatisListViewInit;
    @Autowired
    private MapperCheckBoxInit mapperCheckBoxInit;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);
    @Autowired
    private MybatisExportSetupController mybatisExportSetup1Controller;
    @Autowired
    private ApplicationContext applicationContext;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        NodeConstants.borderPane1 = borderPane;
        NodeConstants.mybatisListView = listView;
        NodeConstants.borderPaneWrap.setCenter(borderPane);
        NodeConstants.mainStackPane = mainStackPane;

        // init mapperCheckBox
        mapperCheckBoxInit.checkBoxInit(mapperCheckBoxHbox1, mapperCheckBoxHbox2);

        mybatisListViewInit.addListener(listView);

        // 入栈
        nodeHandler.addNode(borderPane);
    }

    /**
     * 刷新 table 的字段信息
     */
    @FXML
    public void refreshTableColumn() {
        ObservableList<VBox> selectedItemVboxList = listView.getSelectionModel().getSelectedItems();

        Assert.isTrue(selectedItemVboxList.size() == 1, "请选择一个表进行操作", NodeConstants.primaryStage);

        VBox selectedItemVbox = selectedItemVboxList.get(0);
        String tableName = ((Label) ((HBox) selectedItemVbox.getChildren().get(0)).getChildren().get(0)).getText();
        columnService.reloadColumns(tableName);
        // 如果 size == 2 说明是，闭合状态下点击，如果 > 2 说明是展开状态下点击，这时需要删除
        ObservableList<Node> children = selectedItemVbox.getChildren();
        if (children.size() > 2) {
            selectedItemVbox.getChildren().remove(2);
        }
        mybatisListViewInit.expandTableViewColumns(selectedItemVbox);
    }

    /**
     * 右键高级设置
     */
    @FXML
    public void advancedSetUp() {
        ObservableList<VBox> selectedItemVboxList = listView.getSelectionModel().getSelectedItems();

        Assert.isTrue(selectedItemVboxList.size() == 1, "请选择一个表进行操作", NodeConstants.primaryStage);

        VBox selectedItemVbox = selectedItemVboxList.get(0);
        tableAdvanceSetUpController.openTableAdvancedSetup(NodeConstants.primaryStage, selectedItemVbox);
    }


    @FXML
    public void next() {
        Node next = mybatisExportSetup1Controller.getBorderPane(BaseConstants.selectedDateSource.getConfigName());
        // 入栈
        nodeHandler.addNode(next);

        NodeConstants.borderPaneWrap.setCenter(next);
    }

    /**
     * 获取当前选中的表
     */
    private Table getCurrentSelectedTable() {
        ObservableList<VBox> selectedItemVboxList = listView.getSelectionModel().getSelectedItems();
        Assert.isTrue(selectedItemVboxList.size() == 1, "请选择一个表进行操作", NodeConstants.primaryStage);

        VBox selectedItemVbox = selectedItemVboxList.get(0);
        String tableName = ((Label) ((HBox) selectedItemVbox.getChildren().get(0)).getChildren().get(0)).getText();
        return BaseConstants.selectedTableNameTableMap.get(tableName);
    }

    /**
     * 获取当前选中表的TableView
     */
    private TableView<Column> getCurrentTableTableView() {
        ObservableList<VBox> selectedItemVboxList = listView.getSelectionModel().getSelectedItems();
        Assert.isTrue(selectedItemVboxList.size() == 1, "请选择一个表进行操作", NodeConstants.primaryStage);

        VBox selectedItemVbox = selectedItemVboxList.get(0);
        if (selectedItemVbox.getChildren().size() <= 2) {
            throw new RuntimeException("请先展开表以查看字段");
        }

        // 获取展开的HBox，其中包含TableView
        HBox expandedHBox = (HBox) selectedItemVbox.getChildren().get(2);
        if (expandedHBox.getChildren().isEmpty()) {
            throw new RuntimeException("表字段未正确加载");
        }

        // 获取TableView
        return (TableView<Column>) expandedHBox.getChildren().get(0);
    }

    /**
     * 新增字段
     */
    @FXML
    public void addField() {
        try {
            Table currentTable = getCurrentSelectedTable();
            TableView<Column> tableView = getCurrentTableTableView();

            // 创建新字段，使用默认值
            Column newColumn = new Column();
            newColumn.setColumnName("new_column");
            newColumn.setType("VARCHAR");
            newColumn.setSize(255);
            newColumn.setRemark("新字段");
            newColumn.setNonNullable(false);

            // 添加到表和TableView中
            currentTable.getColumns().add(newColumn);
            tableView.getItems().add(newColumn);

            // 标记为需要保存
            BaseConstants.tableNameIsOverrideRecodeMap.put(currentTable.getTableName(), true);
            
            log.info("成功添加新字段到表: {}", currentTable.getTableName());
        } catch (Exception e) {
            log.error("添加字段失败", e);
            Assert.isTrue(false, "添加字段失败: " + e.getMessage(), NodeConstants.primaryStage);
        }
    }

    /**
     * 删除字段
     */
    @FXML
    public void deleteField() {
        try {
            Table currentTable = getCurrentSelectedTable();
            TableView<Column> tableView = getCurrentTableTableView();

            ObservableList<Column> selectedColumns = tableView.getSelectionModel().getSelectedItems();
            if (selectedColumns.isEmpty()) {
                Assert.isTrue(false, "请至少选择一个字段进行删除", NodeConstants.primaryStage);
                return;
            }

            // 确认删除
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("确认删除");
            alert.setHeaderText(null);
            alert.setContentText("确定要删除选中的 " + selectedColumns.size() + " 个字段吗？");
            alert.initOwner(NodeConstants.primaryStage);

            if (alert.showAndWait().get().getText().equals("确定")) {
                // 从表和TableView中删除选中的字段
                for (Column column : new ArrayList<>(selectedColumns)) {
                    currentTable.getColumns().remove(column);
                    tableView.getItems().remove(column);
                }

                // 标记为需要保存
                BaseConstants.tableNameIsOverrideRecodeMap.put(currentTable.getTableName(), true);
                
                log.info("成功删除 {} 个字段", selectedColumns.size());
            }
        } catch (Exception e) {
            log.error("删除字段失败", e);
            Assert.isTrue(false, "删除字段失败: " + e.getMessage(), NodeConstants.primaryStage);
        }
    }

    /**
     * 保存字段修改
     */
    @FXML
    public void saveFields() {
        try {
            Table currentTable = getCurrentSelectedTable();
            
            // 将当前字段信息保存到磁盘
            columnService.downLoadColumnsToFileSingle(BaseConstants.selectedDateSource, currentTable);
            
            // 从记录映射中移除，表示已保存
            BaseConstants.tableNameIsOverrideRecodeMap.remove(currentTable.getTableName());
            
            log.info("成功保存表 {} 的字段信息", currentTable.getTableName());
            Assert.isTrue(true, "字段信息已保存", NodeConstants.primaryStage);
        } catch (Exception e) {
            log.error("保存字段失败", e);
            Assert.isTrue(false, "保存字段失败: " + e.getMessage(), NodeConstants.primaryStage);
        }
    }

    /**
     * 撤销字段修改
     */
    @FXML
    public void revertFields() {
        try {
            Table currentTable = getCurrentSelectedTable();
            
            // 重新加载字段信息
            columnService.reloadColumns(currentTable.getTableName());
            
            // 从记录映射中移除
            BaseConstants.tableNameIsOverrideRecodeMap.remove(currentTable.getTableName());
            
            // 重新展开表字段视图
            ObservableList<VBox> selectedItemVboxList = listView.getSelectionModel().getSelectedItems();
            VBox selectedItemVbox = selectedItemVboxList.get(0);
            
            // 移除当前展开的字段视图
            if (selectedItemVbox.getChildren().size() > 2) {
                selectedItemVbox.getChildren().remove(2);
            }
            
            // 重新展开
            mybatisListViewInit.expandTableViewColumns(selectedItemVbox);
            
            log.info("成功撤销表 {} 的字段修改", currentTable.getTableName());
            Assert.isTrue(true, "字段修改已撤销", NodeConstants.primaryStage);
        } catch (Exception e) {
            log.error("撤销字段修改失败", e);
            Assert.isTrue(false, "撤销字段修改失败: " + e.getMessage(), NodeConstants.primaryStage);
        }
    }
}