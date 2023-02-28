package com.alan344.controller;

import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.componet.*;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.ConfigConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.DialogFactory;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.FileExploreUtils;
import com.alan344.utils.StringUtils;
import com.alan344.utils.Toast;
import com.jfoenix.controls.JFXCheckBox;
import com.jfoenix.controls.JFXRadioButton;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import lombok.extern.slf4j.Slf4j;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.File;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author AlanSun
 * @date 2019/8/15 17:55
 */
@Slf4j
@Controller
public class MybatisExportSetupController {
    @Resource
    private ConfigService configService;
    @Resource
    private ExportService exportService;
    @Autowired
    private ExtraFileController extraFileController;
    private Map<String, Region> configNameBorderPaneMap = new HashMap<>();

    private ValidationSupport validationSupport = new ValidationSupport();
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);
    private Map<String, LeftRightLinkageBorderPane<MybatisExportConfig, MybatisExportGroupItemHBox>> cache = new HashMap<>();

    private LeftRightLinkageBorderPane<MybatisExportConfig, MybatisExportGroupItemHBox> linkageBorderPane;

    public BorderPane getBorderPane(String configName) {
        linkageBorderPane = cache.computeIfAbsent(configName, s -> {
            final LeftRightLinkageBorderPane<MybatisExportConfig, MybatisExportGroupItemHBox> linkageBorderPane1 = new LeftRightLinkageBorderPane<>(
                    MybatisExportConfig::new,
                    MybatisExportGroupItemHBox::new,
                    this::getExportSetupRegion,
                    NodeConstants.primaryStage,
                    this.getBottomBtns(),
                    0.25
            );

            linkageBorderPane1.addLeftItems(configService.loadConfigFromFile());
            return linkageBorderPane1;
        });
        return linkageBorderPane;
    }

    private List<Button> getBottomBtns() {
        Button openExtraPropertyStageBtn = new Button("添加额外属性");
        openExtraPropertyStageBtn.setOnAction(event -> this.openExtraFileCustomProperties());
        openExtraPropertyStageBtn.setPrefWidth(100);
        Button saveBtn = new Button("保存配置");
        saveBtn.setOnAction(event -> configService.saveConfigToFile());
        saveBtn.setPrefWidth(70);
        Button exportBtn = new Button("导出");
        exportBtn.getStyleClass().add("export");
        exportBtn.setOnAction(event -> this.export());
        exportBtn.setPrefWidth(70);
        Button nextBtn = new Button("下一步");
        nextBtn.setOnAction(event -> this.next());
        nextBtn.setPrefWidth(70);
        Button preBtn = new Button("返回");
        preBtn.setOnAction(event -> this.pre());
        preBtn.setPrefWidth(70);
        return List.of(openExtraPropertyStageBtn, saveBtn, exportBtn, nextBtn, preBtn);
    }

    /**
     * 打开添加自定义属性页面
     */
    public void openExtraFileCustomProperties() {
        final MybatisExportGroupItemHBox selectedItem = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
        if (null == selectedItem) {
            Toast.makeTextDefault(NodeConstants.primaryStage, "请先新增配置");
        }

        final MybatisExportConfig config = selectedItem.getConfig();
        LinkedHashMap<String, String> customProperties = config.getCustomProperties();
        if (null == customProperties) {
            customProperties = new LinkedHashMap<>();
            config.setCustomProperties(customProperties);
        }
        PropertyPane.open(customProperties);
    }

    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    public void next() {
        this.valid();
        final MybatisExportConfig config = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem().getConfig();
        config.setExportExtraFile(true);

        Node next = extraFileController.getBorderPane(config);
        // 入栈
        nodeHandler.addNode(next);

        NodeConstants.borderPaneWrap.setCenter(next);

        // 把作者名称放入全局变量
        ConfigConstants.internalGlobalParam.put("author", config.getAuthor());
    }

    public void saveSetUp() {
        final MybatisExportConfig config = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem().getConfig();
        exportService.saveSetup(config);
        // 保存成功 dialog
        Button configBtn = new Button("打开配置");
        configBtn.setOnAction(event -> FileExploreUtils.open(BaseConstants.MG_CONF_HOME));
        DialogFactory.successDialog(NodeConstants.primaryStage, "保存成功");
    }

    public void export() {
        this.valid();
        final MybatisExportConfig config = linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem().getConfig();
        config.setExportExtraFile(false);
        exportService.export(config);
    }

    public Region getExportSetupRegion(MybatisExportConfig mybatisExportConfig) {
        return configNameBorderPaneMap.computeIfAbsent(mybatisExportConfig.getConfigName(), s -> {
            SplitPane splitPane = new SplitPane();
            splitPane.getStylesheets().add("css/common.css");
            splitPane.setDividerPositions(0.68);
            splitPane.setOrientation(Orientation.VERTICAL);
            ListView<HBox> hBoxListView = new ListView<>();
            hBoxListView.prefWidthProperty().bind(splitPane.widthProperty());

            splitPane.getItems().add(hBoxListView);

            TextField configNameText = new TextField(mybatisExportConfig.getConfigName());
            mybatisExportConfig.configNameProperty().bindBidirectional(configNameText.textProperty());
            MybatisExportItemHBox configNameHbox = new MybatisExportItemHBox("配置名称:", configNameText);
            validationSupport.registerValidator(configNameText, Validator.createEmptyValidator("配置名称必填"));

            TextField authorText = new TextField(mybatisExportConfig.getAuthor());
            mybatisExportConfig.authorProperty().bindBidirectional(authorText.textProperty());
            MybatisExportItemHBox authorHbox = new MybatisExportItemHBox("作者名称:", authorText);
            validationSupport.registerValidator(authorText, Validator.createEmptyValidator("作者名称必填"));

            FileSelectTextToggleHBox beanLocationText = new FileSelectTextToggleHBox("浏览", mybatisExportConfig.modelEnableProperty(), mybatisExportConfig.getBeanLocation());
            mybatisExportConfig.beanLocationProperty().bindBidirectional(beanLocationText.getTextField().textProperty());
            beanLocationText.onAction(event -> this.beanDirectoryScan(beanLocationText));
            MybatisExportItemHBox beanLocationHbox = new MybatisExportItemHBox("Bean 地址:", beanLocationText);

            TextField beanPackageText = new TextField(mybatisExportConfig.getBeanPackage());
            mybatisExportConfig.beanPackageProperty().bindBidirectional(beanPackageText.textProperty());
            MybatisExportItemHBox beanPackageHbox = new MybatisExportItemHBox("Bean 包名:", beanPackageText);

            TextField beanRootClassText = new TextField(mybatisExportConfig.getModelRootClass());
            mybatisExportConfig.modelRootClassProperty().bindBidirectional(beanRootClassText.textProperty());
            MybatisExportItemHBox beanRootClassHbox = new MybatisExportItemHBox("Bean 父类:", beanRootClassText);

            mybatisExportConfig.modelEnableProperty().addListener((observable, oldValue, newValue) -> {
                beanLocationHbox.disable(!newValue);
                beanLocationText.disable(!newValue);
                beanPackageHbox.setDisable(!newValue);
                beanRootClassHbox.setDisable(!newValue);
            });

            FileSelectTextToggleHBox mapperLocationText = new FileSelectTextToggleHBox("浏览", mybatisExportConfig.mapperEnableProperty(), mybatisExportConfig.getMapperLocation());
            mybatisExportConfig.mapperLocationProperty().bindBidirectional(mapperLocationText.getTextField().textProperty());
            mapperLocationText.onAction(event -> this.beanDirectoryScan(mapperLocationText));
            MybatisExportItemHBox mapperLocationHbox = new MybatisExportItemHBox("Mapper 地址:", mapperLocationText);

            TextField mapperPackageText = new TextField(mybatisExportConfig.getMapperPackage());
            mybatisExportConfig.mapperPackageProperty().bindBidirectional(mapperPackageText.textProperty());
            MybatisExportItemHBox mapperPackageHbox = new MybatisExportItemHBox("Mapper 包名:", mapperPackageText);

            TextField mapperRootInterfaceText = new TextField(mybatisExportConfig.getMapperRootInterface());
            mapperRootInterfaceText.setPromptText("非必填，生成的 Mapper 会继承该接口");
            mybatisExportConfig.mapperRootInterfaceProperty().bindBidirectional(mapperRootInterfaceText.textProperty());
            MybatisExportItemHBox mapperRootInterfaceHbox = new MybatisExportItemHBox("Mapper 父接口:", mapperRootInterfaceText);

            mybatisExportConfig.mapperEnableProperty().addListener((observable, oldValue, newValue) -> {
                mapperLocationHbox.disable(!newValue);
                mapperLocationText.disable(!newValue);
                mapperPackageHbox.setDisable(!newValue);
                mapperRootInterfaceHbox.setDisable(!newValue);
            });

            FileSelectTextToggleHBox xmlLocationText = new FileSelectTextToggleHBox("浏览", mybatisExportConfig.xmlEnableProperty(), mybatisExportConfig.getMapperXmlLocation());
            mybatisExportConfig.mapperXmlLocationProperty().bindBidirectional(xmlLocationText.getTextField().textProperty());
            xmlLocationText.onAction(event -> this.beanDirectoryScan(xmlLocationText));
            MybatisExportItemHBox xmlLocationHbox = new MybatisExportItemHBox("Xml 地址:", xmlLocationText);

            mybatisExportConfig.xmlEnableProperty().addListener((observable, oldValue, newValue) -> {
                xmlLocationHbox.disable(!newValue);
                xmlLocationText.disable(!newValue);
            });

            TextField globalIgnoreFieldText = new TextField();
            globalIgnoreFieldText.setPromptText("逗号隔开");
            mybatisExportConfig.globalIgnoreFieldProperty().bindBidirectional(globalIgnoreFieldText.textProperty());
            MybatisExportItemHBox globalIgnoreFieldHbox = new MybatisExportItemHBox("全局忽略字段:", globalIgnoreFieldText);
            hBoxListView.getItems().addAll(configNameHbox, authorHbox, beanLocationHbox, beanPackageHbox, beanRootClassHbox,
                    mapperLocationHbox, mapperPackageHbox, mapperRootInterfaceHbox, xmlLocationHbox, globalIgnoreFieldHbox);

            TabPane tabPane1 = new TabPane();
            tabPane1.setTabClosingPolicy(TabPane.TabClosingPolicy.UNAVAILABLE);
            splitPane.getItems().add(tabPane1);
            tabPane1.selectionModelProperty().addListener((observable, oldValue, newValue) -> {
                final int selectedIndex = newValue.getSelectedIndex();
                mybatisExportConfig.setSelectTab(selectedIndex);
            });
            AnchorPane anchorPane = new AnchorPane();
            Tab tab = new Tab("Official", anchorPane);
            tabPane1.getTabs().add(tab);

            final MybatisExportConfig.MybatisOfficialExportConfig mybatisOfficialExportConfig = mybatisExportConfig.getMybatisOfficialExportConfig();
            JFXCheckBox userJava8CheckBox = new JFXCheckBox("支持 java8");
            userJava8CheckBox.setSelected(mybatisOfficialExportConfig.isUserJava8());
            userJava8CheckBox.setLayoutX(27);
            userJava8CheckBox.setLayoutY(56);
            mybatisOfficialExportConfig.userJava8Property().bindBidirectional(userJava8CheckBox.selectedProperty());
            JFXCheckBox useBigDecimalCheckBox = new JFXCheckBox("使用 BigDecimal");
            useBigDecimalCheckBox.setSelected(mybatisOfficialExportConfig.isUseBigDecimal());
            useBigDecimalCheckBox.setLayoutX(262);
            useBigDecimalCheckBox.setLayoutY(93);
            mybatisOfficialExportConfig.useBigDecimalProperty().bindBidirectional(useBigDecimalCheckBox.selectedProperty());
            JFXCheckBox useCommentCheckBox = new JFXCheckBox("启用注释");
            useCommentCheckBox.setSelected(mybatisOfficialExportConfig.isUseComment());
            useCommentCheckBox.setLayoutX(262);
            useCommentCheckBox.setLayoutY(56);
            mybatisOfficialExportConfig.useCommentProperty().bindBidirectional(useCommentCheckBox.selectedProperty());
            JFXCheckBox useLombokGetSetCheckBox = new JFXCheckBox("启用 lombokGetSet 注解");
            useLombokGetSetCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokGetSet());
            useLombokGetSetCheckBox.setLayoutX(27);
            useLombokGetSetCheckBox.setLayoutY(93);
            mybatisOfficialExportConfig.useLombokGetSetProperty().bindBidirectional(useLombokGetSetCheckBox.selectedProperty());
            JFXCheckBox useLombokBuilderCheckBox = new JFXCheckBox("启用 lombokBuilder 注解");
            useLombokBuilderCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokBuilder());
            useLombokBuilderCheckBox.setLayoutX(27);
            useLombokBuilderCheckBox.setLayoutY(128);
            mybatisOfficialExportConfig.useLombokBuilderProperty().bindBidirectional(useLombokBuilderCheckBox.selectedProperty());

            Label targetNameLabel = new Label("targetName:");
            targetNameLabel.setLayoutX(26);
            targetNameLabel.setLayoutY(20);
            final int selectTab = mybatisExportConfig.getSelectTab();
            tabPane1.getSelectionModel().select(selectTab);

            ToggleGroup toggleGroup = new ToggleGroup();
            toggleGroup.selectedToggleProperty().addListener((observable, oldValue, newValue) -> {
                RadioButton radioButton = (RadioButton) newValue;
                mybatisOfficialExportConfig.setTargetName(radioButton.getText());
            });
            JFXRadioButton mybatis3RadioButton = new JFXRadioButton("Mybatis3");
            mybatis3RadioButton.setSelected(true);
            mybatis3RadioButton.setUserData("Mybatis3");
            mybatis3RadioButton.setToggleGroup(toggleGroup);
            mybatis3RadioButton.setLayoutX(125);
            mybatis3RadioButton.setLayoutY(20);
            JFXRadioButton myBatis3SimpleRadioButton = new JFXRadioButton("MyBatis3Simple");
            myBatis3SimpleRadioButton.setUserData("MyBatis3Simple");
            myBatis3SimpleRadioButton.setToggleGroup(toggleGroup);
            myBatis3SimpleRadioButton.setLayoutX(250);
            myBatis3SimpleRadioButton.setLayoutY(20);
            JFXRadioButton myBatis3DynamicSqlRadioButton = new JFXRadioButton("MyBatis3DynamicSql");
            myBatis3DynamicSqlRadioButton.setUserData("MyBatis3DynamicSql");
            myBatis3DynamicSqlRadioButton.setToggleGroup(toggleGroup);
            myBatis3DynamicSqlRadioButton.setLayoutX(375);
            myBatis3DynamicSqlRadioButton.setLayoutY(20);

            toggleGroup.getToggles().stream()
                    .filter(toggle -> toggle.getUserData().toString().equals(mybatisOfficialExportConfig.getTargetName()))
                    .findFirst().ifPresent(toggleGroup::selectToggle);

            anchorPane.getChildren().addAll(userJava8CheckBox, useBigDecimalCheckBox, useCommentCheckBox,
                    useLombokGetSetCheckBox, useLombokBuilderCheckBox, targetNameLabel, mybatis3RadioButton, myBatis3SimpleRadioButton,
                    myBatis3DynamicSqlRadioButton);
            return splitPane;
        });
    }

    public void valid() {
        if (validationSupport.isInvalid()) {
            Toast.makeTextDefault(NodeConstants.primaryStage, "必填项不能为空");
        }
    }

    //-------------------------文件夹浏览------------------------------------------------------------------------------//

    /**
     * 选择文件夹时，记录上次记录的选择的文件夹，方便用户选择
     */
    private String baseDir;

    /**
     * bean 文件夹选择器
     */
    public void beanDirectoryScan(FileSelectTextHBox fileSelectTextHBox) {
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
        if (directory != null) {
            fileSelectTextHBox.setText(directory.getPath());
            this.baseDir = directory.getPath();
        }
    }

    /**
     * bean 文件夹选择器
     */
    public void beanDirectoryScan(FileSelectTextToggleHBox fileSelectTextHBox) {
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ? null : this.baseDir);
        if (directory != null) {
            fileSelectTextHBox.setText(directory.getPath());
            this.baseDir = directory.getPath();
        }
    }
}
