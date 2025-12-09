import { Context, ModelConfig } from "../types";
import { CreateParamsReturn } from "./createParams";
export type ModifyResult = (context: Context, config: ModelConfig, result: any, params: CreateParamsReturn["params"], ctx?: any) => any;
export declare function modifyReadResult(_: Context, config: ModelConfig, result: any, params: CreateParamsReturn["params"], ctx?: any): CreateParamsReturn;
