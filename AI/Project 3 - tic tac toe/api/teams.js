import qs from 'qs';
import apiClient from "./base.js"
import {teamId} from "./constants.js";

//API call about teams

export const getTeamMembers = () => {
    let params = {
        type: "team",
        teamId
    }
    return apiClient.get("/", { params }).then((response) => response.data)
}

export const createTeam = (name) => {
    const data = qs.stringify({
        'name': name,
        'type': 'team'
    });
    return apiClient.post("/", data).then((response) => response.data)
}

export const addTeamMember = (userId = "1097") => {
    const data = qs.stringify({
        type: 'member',
        userId: userId,
        teamId
    })
    return apiClient.post("/", data).then((response) => response.data)
}