"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.createSoftDeleteExtension = void 0;
const extension_1 = require("@prisma/client/extension");
const prisma_extension_nested_operations_1 = require("@roundtreasury/prisma-extension-nested-operations");
const createParams_1 = require("./helpers/createParams");
const modifyResult_1 = require("./helpers/modifyResult");
function createSoftDeleteExtension({ models, defaultConfig = {
    field: "deleted",
    createValue: Boolean,
    allowToOneUpdates: false,
    allowCompoundUniqueIndexWhere: false,
}, dmmf, }) {
    if (!defaultConfig.field) {
        throw new Error("prisma-extension-soft-delete: defaultConfig.field is required");
    }
    if (!defaultConfig.createValue) {
        throw new Error("prisma-extension-soft-delete: defaultConfig.createValue is required");
    }
    const modelConfig = {};
    Object.keys(models).forEach((model) => {
        const modelName = model;
        const config = models[modelName];
        if (config) {
            modelConfig[modelName] =
                typeof config === "boolean" && config ? defaultConfig : config;
        }
    });
    const context = (0, createParams_1.createContext)(dmmf);
    const createParamsByModel = Object.keys(modelConfig).reduce((acc, model) => {
        const config = modelConfig[model];
        return {
            ...acc,
            [model]: {
                delete: createParams_1.createDeleteParams.bind(null, context, config),
                deleteMany: createParams_1.createDeleteManyParams.bind(null, context, config),
                update: createParams_1.createUpdateParams.bind(null, context, config),
                updateMany: createParams_1.createUpdateManyParams.bind(null, context, config),
                upsert: createParams_1.createUpsertParams.bind(null, context, config),
                findFirst: createParams_1.createFindFirstParams.bind(null, context, config),
                findFirstOrThrow: createParams_1.createFindFirstOrThrowParams.bind(null, context, config),
                findUnique: createParams_1.createFindUniqueParams.bind(null, context, config),
                findUniqueOrThrow: createParams_1.createFindUniqueOrThrowParams.bind(null, context, config),
                findMany: createParams_1.createFindManyParams.bind(null, context, config),
                count: createParams_1.createCountParams.bind(null, context, config),
                aggregate: createParams_1.createAggregateParams.bind(null, context, config),
                where: createParams_1.createWhereParams.bind(null, context, config),
                include: createParams_1.createIncludeParams.bind(null, context, config),
                select: createParams_1.createSelectParams.bind(null, context, config),
                groupBy: createParams_1.createGroupByParams.bind(null, context, config),
            },
        };
    }, {});
    const modifyResultByModel = Object.keys(modelConfig).reduce((acc, model) => {
        const config = modelConfig[model];
        return {
            ...acc,
            [model]: {
                include: modifyResult_1.modifyReadResult.bind(null, context, config),
                select: modifyResult_1.modifyReadResult.bind(null, context, config),
            },
        };
    }, {});
    // before handling root params generate deleted value so it is consistent
    // for the query. Add it to root params and get it from scope?
    return extension_1.Prisma.defineExtension((client) => {
        return client.$extends({
            query: {
                $allModels: {
                    // @ts-expect-error - we don't know what the client is
                    $allOperations: (0, prisma_extension_nested_operations_1.withNestedOperations)({
                        dmmf,
                        async $rootOperation(initialParams) {
                            var _a, _b;
                            const createParams = (_a = createParamsByModel[initialParams.model || ""]) === null || _a === void 0 ? void 0 : _a[initialParams.operation];
                            if (!createParams)
                                return initialParams.query(initialParams.args);
                            const { params, ctx } = createParams(initialParams);
                            const { model } = params;
                            const operationChanged = params.operation !== initialParams.operation;
                            const result = operationChanged
                                ? // @ts-expect-error - we don't know what the client is
                                    await client[model[0].toLowerCase() + model.slice(1)][params.operation](params.args)
                                : await params.query(params.args);
                            const modifyResult = (_b = modifyResultByModel[params.model || ""]) === null || _b === void 0 ? void 0 : _b[params.operation];
                            if (!modifyResult)
                                return result;
                            return modifyResult(result, params, ctx);
                        },
                        async $allNestedOperations(initialParams) {
                            var _a, _b;
                            const createParams = (_a = createParamsByModel[initialParams.model || ""]) === null || _a === void 0 ? void 0 : _a[initialParams.operation];
                            if (!createParams)
                                return initialParams.query(initialParams.args);
                            const { params, ctx } = createParams(initialParams);
                            const result = await params.query(params.args, params.operation);
                            const modifyResult = (_b = modifyResultByModel[params.model || ""]) === null || _b === void 0 ? void 0 : _b[params.operation];
                            if (!modifyResult)
                                return result;
                            return modifyResult(result, params, ctx);
                        },
                    }),
                },
            },
        });
    });
}
exports.createSoftDeleteExtension = createSoftDeleteExtension;
