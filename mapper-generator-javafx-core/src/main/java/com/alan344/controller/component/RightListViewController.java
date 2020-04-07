package com.alan344.controller.component;

import com.alan344.constants.StageConstants;
import com.alan344.controller.TableAdvanceSetUpController;
import com.alan344.init.RightListViewInit;
import com.alan344.service.ColumnService;
import com.alan344.utils.Assert;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import lombok.Getter;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/4/7 17:09
 */
@Controller
public class RightListViewController implements Initializable {
    @Getter
    @FXML
    private ListView<VBox> vBoxListView;

    @Resource
    private TableAdvanceSetUpController tableAdvanceSetUpController;

    @Resource
    private ColumnService columnService;

    @Resource
    private RightListViewInit rightListViewInit;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        rightListViewInit.addListener(vBoxListView);
    }

    /**
     * 刷新 table 的字段信息
     */
    @FXML
    public void refreshTableColumn() {
        ObservableList<VBox> selectedItemVBoxs = vBoxListView.getSelectionModel().getSelectedItems();

        Assert.isTrue(selectedItemVBoxs.size() == 1, "请选择一个表进行操作", StageConstants.primaryStage);

        VBox selectedItemVBox = selectedItemVBoxs.get(0);
        String tableName = ((Label) ((HBox) selectedItemVBox.getChildren().get(0)).getChildren().get(0)).getText();
        columnService.reloadColumns(tableName);
        // 如果 size == 2 说明是，闭合状态下点击，如果 > 2 说明是展开状态下点击，这时需要删除
        ObservableList<Node> children = selectedItemVBox.getChildren();
        if (children.size() > 2) {
            selectedItemVBox.getChildren().remove(2);
        }
        rightListViewInit.expandTableViewColumns(selectedItemVBox);
    }

    /**
     * 高级设置
     */
    @FXML
    public void advancedSetUp() throws IOException {
        ObservableList<VBox> selectedItemVBoxs = vBoxListView.getSelectionModel().getSelectedItems();

        Assert.isTrue(selectedItemVBoxs.size() == 1, "请选择一个表进行操作", StageConstants.primaryStage);

        VBox selectedItemVBox = selectedItemVBoxs.get(0);
        tableAdvanceSetUpController.openTableAdvancedSetUP(StageConstants.primaryStage, selectedItemVBox);
    }
}
