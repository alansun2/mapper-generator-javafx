package com.alan344.controller;

import com.alan344.bean.Table;
import com.alan344.constants.BaseConstants;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/4/6 16:21
 */
@Controller
public class TableAdvanceSetUpController implements Initializable {

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
     * @throws IOException e
     */
    public void openTableAdvancedSetUP(Stage primaryStage, VBox selectedItemVBox) throws IOException {
        if (tableAdvanceSetUpStage == null) {
            FXMLLoader fxmlLoader = new FXMLLoader();

            fxmlLoader.setLocation(getClass().getResource("/fxml/table-advanced-set-up.fxml"));
            fxmlLoader.setControllerFactory(beanFactory::getBean);

            AnchorPane tableAdvanceSetUpPane = fxmlLoader.load();
            tableAdvanceSetUpStage = new Stage();
            tableAdvanceSetUpStage.setScene(new Scene(tableAdvanceSetUpPane));
            tableAdvanceSetUpStage.setTitle("高级设置");
            tableAdvanceSetUpStage.getIcons().add(new Image("/image/advanced-set-up.png"));
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
