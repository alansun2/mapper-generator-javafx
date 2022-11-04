package com.alan344.controller;

import com.alan344.constants.NodeConstants;
import com.alan344.controller.component.TableAdvanceSetupController;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.init.MapperCheckBoxInit;
import com.alan344.init.MybatisListViewInit;
import com.alan344.service.ColumnService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.Assert;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/4/7 17:09
 */
@Controller
public class MybatisListViewController implements Initializable {
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
    private HBox mapperCheckBoxHBox1;
    @FXML
    private HBox mapperCheckBoxHBox2;
    @Resource
    private TableAdvanceSetupController tableAdvanceSetUpController;
    @Resource
    private ColumnService columnService;
    @Resource
    private MybatisListViewInit mybatisListViewInit;
    @Resource
    private BeanFactory beanFactory;
    @Resource
    private MapperCheckBoxInit mapperCheckBoxInit;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        NodeConstants.borderPane1 = borderPane;
        NodeConstants.mybatisListView = listView;
        NodeConstants.borderPaneWrap.setCenter(borderPane);
        NodeConstants.mainStackPane = mainStackPane;

        // init mapperCheckBox
        mapperCheckBoxInit.checkBoxInit(mapperCheckBoxHBox1, mapperCheckBoxHBox2);

        mybatisListViewInit.addListener(listView);

        // 入栈
        nodeHandler.addNode(borderPane);
    }

    /**
     * 刷新 table 的字段信息
     */
    @FXML
    public void refreshTableColumn() {
        ObservableList<VBox> selectedItemVBoxs = listView.getSelectionModel().getSelectedItems();

        Assert.isTrue(selectedItemVBoxs.size() == 1, "请选择一个表进行操作", NodeConstants.primaryStage);

        VBox selectedItemVBox = selectedItemVBoxs.get(0);
        String tableName = ((Label) ((HBox) selectedItemVBox.getChildren().get(0)).getChildren().get(0)).getText();
        columnService.reloadColumns(tableName);
        // 如果 size == 2 说明是，闭合状态下点击，如果 > 2 说明是展开状态下点击，这时需要删除
        ObservableList<Node> children = selectedItemVBox.getChildren();
        if (children.size() > 2) {
            selectedItemVBox.getChildren().remove(2);
        }
        mybatisListViewInit.expandTableViewColumns(selectedItemVBox);
    }

    /**
     * 右键高级设置
     */
    @FXML
    public void advancedSetUp() {
        ObservableList<VBox> selectedItemVBoxs = listView.getSelectionModel().getSelectedItems();

        Assert.isTrue(selectedItemVBoxs.size() == 1, "请选择一个表进行操作", NodeConstants.primaryStage);

        VBox selectedItemVBox = selectedItemVBoxs.get(0);
        tableAdvanceSetUpController.openTableAdvancedSetup(NodeConstants.primaryStage, selectedItemVBox);
    }


    @FXML
    public void next() {
        Node next = nodeHandler.getNext();
        if (next == null) {
            next = FxmlLoadFactory.create("/fxml/mybatis-setup.fxml", beanFactory);
            // 入栈
            nodeHandler.addNode(next);
        }

        NodeConstants.borderPaneWrap.setCenter(next);
    }
}
