package com.alan344.controller;

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
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/4/7 17:09
 */
@Controller
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
    @Autowired
    private MybatisExportSetupController mybatisExportSetup1Controller;

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
        Node next = nodeHandler.getNext();
        if (next == null) {

            // next = FxmlLoadFactory.create("/fxml/mybatis-export-setup.fxml", beanFactory);
            next = mybatisExportSetup1Controller.getBorderPane(BaseConstants.selectedDateSource.getConfigName());
            // 入栈
            nodeHandler.addNode(next);
        }

        NodeConstants.borderPaneWrap.setCenter(next);
    }
}
