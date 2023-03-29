package com.alan344.controller;

import com.alan344.bean.DataSource;
import com.alan344.constants.enums.DriverEnum;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.init.DataSourceTreeItemInit;
import com.alan344.service.DataSourceService;
import com.alan344.utils.Assert;
import com.alan344.utils.StringUtils;
import com.jfoenix.controls.JFXComboBox;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.Getter;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/8 13:24
 */
@Component
public class DataSourceSetupController implements Initializable {
    @FXML
    private TextField configNameTextField;
    @FXML
    private TextField urlTextField;
    @Getter
    @FXML
    private TextField userTextField;
    @Getter
    @FXML
    private TextField passwordTextField;
    @Getter
    @FXML
    private TextField driveNameTextField;
    @FXML
    private JFXComboBox<DriverEnum> driveTypeComboBox;
    @FXML
    private Button testConnectionBtn;

    //-----------------------------------------

    @Resource
    private ApplicationContext applicationContext;
    @Resource
    private MainController mainController;
    @Resource
    private DataSourceService dataSourceService;
    @Resource
    private DataSourceTreeItemInit dataSourceTreeItemInit;
    private Stage dateSourceStage;
    /**
     * 判断是添加数据源还是更新数据源
     */
    private boolean isAdd = true;
    /**
     * 旧数据源, 用于更新数据源
     */
    private DataSource oldDataSource;
    /**
     * 当前数据源
     */
    private DataSource curDataSource;

    private ValidationSupport validationSupport;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        driveTypeComboBox.getItems().addAll(DriverEnum.values());
        driveTypeComboBox.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue != null) {
                if (StringUtils.isEmpty(driveNameTextField.getText())) {
                    driveNameTextField.setText(newValue.getDriveName());
                }
                if (StringUtils.isEmpty(urlTextField.getText())) {
                    urlTextField.setText(newValue.getDefaultUrl());
                }
            }
        });


        validationSupport = new ValidationSupport();
        validationSupport.setValidationDecorator(new StyleClassValidationDecoration());
        validationSupport.registerValidator(configNameTextField, Validator.createEmptyValidator("配置名称不能为空"));
        validationSupport.registerValidator(urlTextField, Validator.createEmptyValidator("url不能为空"));
        validationSupport.registerValidator(userTextField, Validator.createEmptyValidator("user不能为空"));
        validationSupport.registerValidator(passwordTextField, Validator.createEmptyValidator("password不能为空"));
        validationSupport.registerValidator(driveNameTextField, Validator.createEmptyValidator("driveName不能为空"));
    }

    /**
     * 应用后 添加数据源
     *
     * @throws IOException e
     */
    @FXML
    public void apply() throws IOException {
        Assert.isTrue(!validationSupport.isInvalid(), "请填写完整数据源信息", dateSourceStage);
        if (isAdd) {
            // 判断数据源是否存在
            Assert.isTrue(!dataSourceService.contains(curDataSource), "该数据源已存在", dateSourceStage);

            // 添加数据源
            dataSourceService.addDataSource(curDataSource);

            // 把 dataSource 放入 treeItemRoot
            dataSourceTreeItemInit.addExpandListenerForDataSource(curDataSource, mainController.getTreeItemDataSourceRoot());

            // 点击应用后关闭添加数据源页面
            dateSourceStage.close();
        } else {
            // 更新数据源
            dataSourceService.updateDataSource(oldDataSource, curDataSource);

            // 点击应用后关闭添加数据源页面
            dateSourceStage.close();
        }
    }

    /**
     * 关闭窗口
     */
    @FXML
    public void close() {
        dateSourceStage.close();
    }

    /**
     * 测试连接
     */
    @FXML
    public void testConnection() {
        Assert.isTrue(!validationSupport.isInvalid(), "请填写完整数据源信息", dateSourceStage);
        if (dataSourceService.testConnection(curDataSource)) {
            testConnectionBtn.setStyle("-fx-background-color: #cafdca");
        } else {
            testConnectionBtn.setStyle("-fx-background-color: #fab5b5");
        }
    }

    /**
     * 添加数据源的 stage
     *
     * @param primaryStage 主窗口
     */
    public void openDataSourceSetUp(Stage primaryStage, DataSource dataSource) {
        dateSourceStage = new Stage();
        dateSourceStage.setScene(new Scene(FxmlLoadFactory.create("/fxml/datasource-setup.fxml", applicationContext)));
        dateSourceStage.setResizable(false);
        dateSourceStage.getIcons().add(new Image("/image/icon.png"));
        dateSourceStage.setTitle("设置数据源");
        dateSourceStage.initModality(Modality.WINDOW_MODAL);
        dateSourceStage.initOwner(primaryStage);
        if (null != dataSource) {
            isAdd = false;
            oldDataSource = dataSource.copy();
        } else {
            isAdd = true;
            dataSource = new DataSource();
        }
        curDataSource = dataSource;
        configNameTextField.textProperty().bindBidirectional(dataSource.configNameProperty());
        urlTextField.textProperty().bindBidirectional(dataSource.urlProperty());
        userTextField.textProperty().bindBidirectional(dataSource.userProperty());
        passwordTextField.textProperty().bindBidirectional(dataSource.passwordProperty());
        driveNameTextField.textProperty().bindBidirectional(dataSource.driveNameProperty());
        dateSourceStage.show();
    }
}
