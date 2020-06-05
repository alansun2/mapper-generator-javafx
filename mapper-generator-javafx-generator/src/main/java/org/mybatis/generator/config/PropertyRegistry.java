/**
 *    Copyright 2006-2019 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package org.mybatis.generator.config;

/**
 * This class holds constants for all properties recognized by the different
 * configuration elements. This helps document and maintain the different
 * properties, and helps to avoid spelling errors.
 * 
 * @author Jeff Butler
 * 
 */
public class PropertyRegistry {
    private PropertyRegistry() {}
    
    public static final String ANY_ENABLE_SUB_PACKAGES = "enableSubPackages";

    /**
     * recognized by table and java model generator.
     */
    public static final String ANY_ROOT_CLASS = "rootClass";
    public static final String ANY_IMMUTABLE = "immutable";
    public static final String ANY_CONSTRUCTOR_BASED = "constructorBased";

    /**
     * recognized by table and java client generator.
     */
    public static final String ANY_ROOT_INTERFACE = "rootInterface";

    public static final String TABLE_USE_COLUMN_INDEXES = "useColumnIndexes";
    public static final String TABLE_USE_ACTUAL_COLUMN_NAMES = "useActualColumnNames";
    public static final String TABLE_USE_COMPOUND_PROPERTY_NAMES = "useCompoundPropertyNames";
    public static final String TABLE_IGNORE_QUALIFIERS_AT_RUNTIME = "ignoreQualifiersAtRuntime";
    public static final String TABLE_RUNTIME_CATALOG = "runtimeCatalog";
    public static final String TABLE_RUNTIME_SCHEMA = "runtimeSchema";
    public static final String TABLE_RUNTIME_TABLE_NAME = "runtimeTableName";
    public static final String TABLE_MODEL_ONLY = "modelOnly";
    public static final String TABLE_SELECT_ALL_ORDER_BY_CLAUSE = "selectAllOrderByClause";

    public static final String CONTEXT_BEGINNING_DELIMITER = "beginningDelimiter";
    public static final String CONTEXT_ENDING_DELIMITER = "endingDelimiter";
    public static final String CONTEXT_AUTO_DELIMIT_KEYWORDS = "autoDelimitKeywords";
    public static final String CONTEXT_JAVA_FILE_ENCODING = "javaFileEncoding";
    public static final String CONTEXT_JAVA_FORMATTER = "javaFormatter";
    public static final String CONTEXT_XML_FORMATTER = "xmlFormatter";
    public static final String CONTEXT_TARGET_JAVA8 = "targetJava8";

    public static final String CLIENT_USE_LEGACY_BUILDER = "useLegacyBuilder";
    
    public static final String TYPE_RESOLVER_FORCE_BIG_DECIMALS = "forceBigDecimals";
    public static final String TYPE_RESOLVER_USE_JSR310_TYPES = "useJSR310Types";

    public static final String MODEL_GENERATOR_TRIM_STRINGS = "trimStrings";
    public static final String MODEL_GENERATOR_EXAMPLE_PACKAGE = "exampleTargetPackage";
    public static final String MODEL_GENERATOR_EXAMPLE_PROJECT = "exampleTargetProject";

    public static final String COMMENT_GENERATOR_SUPPRESS_DATE = "suppressDate";
    public static final String COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS = "suppressAllComments";
    public static final String COMMENT_GENERATOR_ADD_REMARK_COMMENTS = "addRemarkComments";
    public static final String COMMENT_GENERATOR_DATE_FORMAT = "dateFormat";
}
