package com.alan344.controller.component;

import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import com.alan344.factory.FxmlLoadFactory;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/4/6 16:21
 * <p>
 * 表格高级设置
 */
@Controller
public class TableAdvanceSetupController implements Initializable {

    @FXML
    private CheckBox serializableCheckBox;

    @Resource
    private BeanFactory beanFactory;

    private Stage tableAdvanceSetUpStage;

    private Table selectedTable;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
    }

    /**
     * 打开表高级设置
     *
     * @param primaryStage 主窗口
     */
    public void openTableAdvancedSetup(Stage primaryStage, VBox selectedItemVBox) {
        if (tableAdvanceSetUpStage == null) {
            tableAdvanceSetUpStage = new Stage();
            tableAdvanceSetUpStage.setScene(new Scene(FxmlLoadFactory.create("/fxml/table-advanced-setup.fxml", beanFactory)));
            tableAdvanceSetUpStage.setTitle("高级设置");
            tableAdvanceSetUpStage.getIcons().add(new Image("/image/icon.png"));
            tableAdvanceSetUpStage.setResizable(false);
            tableAdvanceSetUpStage.initModality(Modality.WINDOW_MODAL);
            tableAdvanceSetUpStage.initOwner(primaryStage);
        }

        tableAdvanceSetUpStage.show();

        String tableName = ((Label) ((HBox) selectedItemVBox.getChildren().get(0)).getChildren().get(0)).getText();
        selectedTable = BaseConstants.selectedTableNameTableMap.get(tableName);
        serializableCheckBox.setSelected(selectedTable.isJdkSerializable());
    }

    @FXML
    public void cancel() {
        tableAdvanceSetUpStage.close();
    }

    @FXML
    public void apply() {
        if (serializableCheckBox.selectedProperty().get()) {
            selectedTable.setJdkSerializable(true);
            BaseConstants.tableNameSetUpTableRecordMap.put(selectedTable.getTableName(), true);
        } else {
            selectedTable.setJdkSerializable(false);
            BaseConstants.tableNameSetUpTableRecordMap.remove(selectedTable.getTableName());
        }
        tableAdvanceSetUpStage.close();
    }
}
