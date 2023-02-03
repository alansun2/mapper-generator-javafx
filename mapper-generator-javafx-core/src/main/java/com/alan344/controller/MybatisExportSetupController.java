package com.alan344.controller;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.controller.component.MybatisExportController;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.BorderPane;
import lombok.extern.slf4j.Slf4j;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/15 17:55
 */
@Slf4j
@Controller
public class MybatisExportSetupController implements Initializable {
    @FXML
    private BorderPane setUpListBoardPane;
    @FXML
    private SplitPane splitPane;
    @FXML
    private ListView<String> selectConfigLV;
    @Resource
    private ConfigService configService;
    @Resource
    private BeanFactory beanFactory;
    @Resource
    private MybatisExportController mybatisExportController;
    @Resource
    private ExportService exportService;
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        BorderPane borderPane = FxmlLoadFactory.create("/fxml/component/mybatis-export-setup-main.fxml", beanFactory);

        splitPane.getItems().add(borderPane);

        // 加载配置文件
        List<MybatisExportConfig> mybatisExportConfigs = configService.loadConfigFromFile();
        if (!mybatisExportConfigs.isEmpty()) {
            mybatisExportConfigs.forEach(this::addConfigButton);
            // 显示第一个config
            mybatisExportController.showConfig(mybatisExportConfigs.get(0));
            selectConfigLV.getSelectionModel().select(0);
        }

        final Map<String, MybatisExportConfig> configNameConfigMap = configService.getConfigNameConfigMap();
        setUpListBoardPane.setCenter(selectConfigLV);
        // 设置 listview 单选
        selectConfigLV.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        // 设置点击事件
        selectConfigLV.setOnMouseClicked(event -> {
            if (event.getButton() == MouseButton.PRIMARY) {
                mybatisExportController.showConfig(configNameConfigMap.get(((ListView<String>) event.getSource()).getSelectionModel().getSelectedItem()));
            }
        });

        // 添加右键功能
        MenuItem addMenuItem = new MenuItem("添加");
        addMenuItem.setOnAction(event -> this.addEmptyExportPane());
        addMenuItem.setGraphic(new FontIcon("unil-plus-circle:16:BLUE"));
        MenuItem delMenuItem = new MenuItem("删除");
        delMenuItem.setGraphic(new FontIcon("unil-times-circle:16:RED"));
        delMenuItem.setOnAction(event -> {
            final String selectedItem = selectConfigLV.getSelectionModel().getSelectedItem();
            this.deleteConfig(selectedItem, configNameConfigMap.get(selectedItem));
        });
        selectConfigLV.setContextMenu(new ContextMenu(addMenuItem, delMenuItem));
    }

    /**
     * 删除配置
     *
     * @param mybatisExportConfig 配置信息
     */
    private void deleteConfig(String configName, MybatisExportConfig mybatisExportConfig) {
        final ObservableList<String> items = selectConfigLV.getItems();
        int size = items.size();
        items.remove(configName);
        if (size == 1) {
            mybatisExportController.clearPane();
        } else {
            mybatisExportController.showConfig(configService.getConfigNameConfigMap().get(items.get(0)));
        }

        configService.deleteConfig(mybatisExportConfig);
    }

    /**
     * 添加配置文件有到左边的 listview
     *
     * @param mybatisExportConfig 配置信息
     */
    private void addConfigButton(MybatisExportConfig mybatisExportConfig) {
        selectConfigLV.getItems().add(mybatisExportConfig.getConfigName());
    }

    /**
     * 新增一个配置
     */
    @FXML
    public void addEmptyExportPane() {
        mybatisExportController.clearPane();
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    @FXML
    public void next() {
        mybatisExportController.validExport();

        Node next = FxmlLoadFactory.create("/fxml/extra-file.fxml", beanFactory);
        // 入栈
        nodeHandler.addNode(next);

        final MybatisExportConfig config = mybatisExportController.getConfig(BaseConstants.currentConfig);
        config.setExportExtraFile(true);
        NodeConstants.borderPaneWrap.setCenter(next);
    }

    @FXML
    public void saveSetUp() {
        mybatisExportController.validExport();
        final MybatisExportConfig config = mybatisExportController.getConfig(BaseConstants.currentConfig);
        exportService.saveSetup(config);
    }

    @FXML
    public void export() {
        mybatisExportController.validExport();
        final MybatisExportConfig config = mybatisExportController.getConfig(BaseConstants.currentConfig);
        config.setExportExtraFile(false);
        exportService.export(config);
    }
}
