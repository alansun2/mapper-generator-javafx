package com.alan344.controller;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.ZipUtil;
import com.alan344.bean.DataItem;
import com.alan344.component.CustomTreeCell;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.DialogFactory;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.init.DataSourceTreeItemInit;
import com.alan344.init.DataSourceTreeViewInit;
import com.alan344.init.FindTableInit;
import com.alan344.utils.Assert;
import com.alan344.utils.FileExploreUtils;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.controlsfx.control.textfield.TextFields;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.File;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/7 17:04
 */
@Slf4j
@Controller
public class MainController implements Initializable {
    @FXML
    private StackPane mainStackPane;
    /**
     * 主布局控件
     */
    @Getter
    @FXML
    private BorderPane borderPaneMain;

    /**
     * 右边的父 border
     */
    @FXML
    private BorderPane borderPaneWrap;

    @FXML
    private MenuBar menuBar;

    /**
     * 左边的 treeView
     */
    @Getter
    @FXML
    private TreeView<DataItem> treeViewDataSource;

    /**
     * 根数据源
     */
    @Getter
    @FXML
    private TreeItem<DataItem> treeItemDataSourceRoot;

    /**
     * 搜索表时用的 TextField
     */
    @FXML
    private Label searchLabel;
    @Getter
    private TextField tableFindTextField;
    @FXML
    private HBox tableFindTextFieldHbox;

    //-------------------------------service----------------------------------------------------------------------------

    @Resource
    private DataSourceSetupController dataSourceSetupController;

    @Resource
    private AboutController aboutController;

    @Resource
    private BeanFactory beanFactory;

    // -------------------------init----------------------------------------------------------------------------------//

    @Resource
    private DataSourceTreeItemInit dataSourceTreeItemInit;

    @Resource
    private DataSourceTreeViewInit dataSourceTreeViewInit;

    @Resource
    private FindTableInit findTableInit;

    //--------------------------------init----------------------------------------------------------------------//

    @SneakyThrows
    @Override
    public void initialize(URL location, ResourceBundle resources) {
        NodeConstants.borderPaneWrap = borderPaneWrap;
        // 把菜单的长度和主布局控件绑定
        menuBar.prefWidthProperty().bind(borderPaneMain.widthProperty());
        menuBar.setOnMousePressed(this::handleMousePressed);
        menuBar.setOnMouseDragged(this::handleMouseDragged);

        tableFindTextField = TextFields.createClearableTextField();
        tableFindTextField.setStyle("-fx-background-color: #FFF;-fx-border-width: 0;-fx-border-color: #FFF;");
        tableFindTextField.setPromptText("Table Filter");
        tableFindTextField.setFocusTraversable(false);
        tableFindTextFieldHbox.getChildren().add(tableFindTextField);
        searchLabel.prefHeightProperty().bind(tableFindTextFieldHbox.heightProperty());
        tableFindTextField.prefHeightProperty().bind(tableFindTextFieldHbox.heightProperty());
        tableFindTextField.prefWidthProperty().bind(tableFindTextFieldHbox.widthProperty());

        treeViewDataSource.setCellFactory(CustomTreeCell.forTreeView());
        dataSourceTreeViewInit.treeViewInit(treeViewDataSource);

        // 从文件加载数据源至pane
        dataSourceTreeItemInit.initLoadData(treeItemDataSourceRoot);

        // 添加表搜索监听
        findTableInit.addListener(treeViewDataSource, tableFindTextField, borderPaneWrap);

        // 加载右边中间的 borderPane
        FxmlLoadFactory.create("/fxml/mybatis-table-setup.fxml", beanFactory);
    }

    private double dragOffsetX;

    private double dragOffsetY;

    protected void handleMousePressed(MouseEvent e) {
        // Store the mouse x and y coordinates with respect to the
        // stage in the reference variables to use them in the drag event
        // 点击鼠标时，获取鼠标在窗体上点击时相对应窗体左上角的偏移
        this.dragOffsetX = e.getScreenX() - NodeConstants.primaryStage.getX();
        this.dragOffsetY = e.getScreenY() - NodeConstants.primaryStage.getY();
    }

    protected void handleMouseDragged(MouseEvent e) {
        // Move the stage by the drag amount
        // 拖动鼠标后，获取鼠标相对应显示器坐标减去鼠标相对窗体的坐标，并将其设置为窗体在显示器上的坐标
        NodeConstants.primaryStage.setX(e.getScreenX() - this.dragOffsetX);
        NodeConstants.primaryStage.setY(e.getScreenY() - this.dragOffsetY);
    }

    //--------------------------------init end------------------------------------------------------------------------//

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() {
        dataSourceSetupController.openDataSourceSetUp(NodeConstants.primaryStage, treeViewDataSource, null);
    }

    /**
     * 退出时弹出询问
     */
    @FXML
    public void exit() {
        DialogFactory.closeDialog(NodeConstants.primaryStage, mainStackPane);
    }

    @FXML
    public void openConfigDir() {
        FileExploreUtils.open(BaseConstants.MG_CONF_HOME);
    }

    @FXML
    public void importConfig() {
        // 选择文件
        final File fileScan = FileDirChooserFactory.createFileScan("选择zip文件", BaseConstants.baseFileDir, "配置文件", "*.zip");
        if (null != fileScan) {
            // 先备份
            if (FileUtil.exist(BaseConstants.MG_CONF_HOME)) {
                ZipUtil.zip(BaseConstants.MG_CONF_HOME, BaseConstants.MG_HOME + "/config-backup.zip", Charset.defaultCharset(), true);
            }
            BaseConstants.baseFileDir = fileScan.getParent().replace("\\", "/");
            ZipUtil.unzip(fileScan, new File(BaseConstants.MG_HOME), Charset.defaultCharset());
            // 弹框
            DialogFactory.successDialog(NodeConstants.primaryStage, "导出成功");
            // 从文件加载数据源至pane
            dataSourceTreeItemInit.initLoadData(treeItemDataSourceRoot);
        }
    }

    @FXML
    public void exportConfig() {
        Assert.isTrue(FileUtil.exist(BaseConstants.MG_CONF_HOME), "暂无可以导出的配置", NodeConstants.primaryStage);
        final File directory = FileDirChooserFactory.createDirectoryScan("导出文件名称", null);
        if (null != directory) {
            final String absolutePath = directory.getAbsolutePath().replace("\\", "/");
            ZipUtil.zip(BaseConstants.MG_CONF_HOME, absolutePath + "/config.zip", Charset.defaultCharset(), true);
            // 弹框
            DialogFactory.successAndOpenFileDialog(NodeConstants.primaryStage, "导出成功", absolutePath);
        }
    }

    /**
     * 点开项目地址
     */
    @FXML
    public void openGithub() {
        NodeConstants.hostServices.showDocument("https://github.com/alansun2/mapper-generator-javafx");
    }

    /**
     * 关于窗口
     */
    @FXML
    public void openAboutWindow() {
        aboutController.openWindow(NodeConstants.primaryStage);
    }
}
